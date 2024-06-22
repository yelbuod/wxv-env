/** *************************************************************************************
 * Copyright (c) 2024-2026 YangYang, https://github.com/yelbuod
 *
 * WenXuanVec is licensed under Mulan PSL v2.
 * You can use this software according to the terms and conditions of the Mulan PSL v2.
 * You may obtain a copy of Mulan PSL v2 at:
 * http://license.coscl.org.cn/MulanPSL2
 *
 * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
 * EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
 * MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
 *
 * See the Mulan PSL v2 for more details.
 * ************************************************************************************* */

package wenxuan.backend.decode

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import wenxuan.backend.{CtrlSignals, InstCtrlPacket, MicroOp, CustomCSRCtrlIO}
import wenxuan.common.{InstPacket, WXModule}
import freechips.rocketchip.rocket.Instructions._
import freechips.rocketchip.util.uintToBitPat
import utility.{LookupTree, SignExt, ZeroExt}
import utils.XSDebug
import wenxuan.commonType.{FuType, SelImm, SrcType}
import wenxuan.functionOpType._
import wenxuan.backendInfoType.ExceptionNO.illegalInstr

abstract class Imm(val len: Int) extends Bundle {
  def toImm32(minBits: UInt): UInt = do_toImm32(minBits(len - 1, 0))
  def do_toImm32(minBits: UInt): UInt
  def minBitsFromInstr(instr: UInt): UInt
}

case class Imm_I() extends Imm(12) {
  override def do_toImm32(minBits: UInt): UInt = SignExt(minBits(len - 1, 0), 32)

  override def minBitsFromInstr(instr: UInt): UInt =
    Cat(instr(31, 20))
}

case class Imm_S() extends Imm(12) {
  override def do_toImm32(minBits: UInt): UInt = SignExt(minBits, 32)

  override def minBitsFromInstr(instr: UInt): UInt =
    Cat(instr(31, 25), instr(11, 7))
}

case class Imm_B() extends Imm(12) {
  override def do_toImm32(minBits: UInt): UInt = SignExt(Cat(minBits, 0.U(1.W)), 32)

  override def minBitsFromInstr(instr: UInt): UInt =
    Cat(instr(31), instr(7), instr(30, 25), instr(11, 8))
}

case class Imm_U() extends Imm(20){
  override def do_toImm32(minBits: UInt): UInt = Cat(minBits(len - 1, 0), 0.U(12.W))

  override def minBitsFromInstr(instr: UInt): UInt = {
    instr(31, 12)
  }
}

case class Imm_J() extends Imm(20){
  override def do_toImm32(minBits: UInt): UInt = SignExt(Cat(minBits, 0.U(1.W)), 32)

  override def minBitsFromInstr(instr: UInt): UInt = {
    Cat(instr(31), instr(19, 12), instr(20), instr(30, 25), instr(24, 21))
  }
}

case class Imm_Z() extends Imm(12 + 5){
  override def do_toImm32(minBits: UInt): UInt = minBits

  override def minBitsFromInstr(instr: UInt): UInt = {
    Cat(instr(19, 15), instr(31, 20))
  }
}

case class Imm_B6() extends Imm(6){
  override def do_toImm32(minBits: UInt): UInt = ZeroExt(minBits, 32)

  override def minBitsFromInstr(instr: UInt): UInt = {
    instr(25, 20)
  }
}

object ImmUnion {
  val I = Imm_I()
  val S = Imm_S()
  val B = Imm_B()
  val U = Imm_U()
  val J = Imm_J()
  val Z = Imm_Z()
  val B6 = Imm_B6()
  val imms = Seq(I, S, B, U, J, Z, B6)
  val maxLen = imms.maxBy(_.len).len
  val immSelMap = Seq(
    SelImm.IMM_I,
    SelImm.IMM_S,
    SelImm.IMM_SB,
    SelImm.IMM_U,
    SelImm.IMM_UJ,
    SelImm.IMM_Z,
    SelImm.IMM_B6
  ).zip(imms)
  println(s"ImmUnion max len: $maxLen")
}

trait RVDecodeConstants
{
  // abstract out instruction decode magic numbers
  val RD_MSB  = 11
  val RD_LSB  = 7
  val RS1_MSB = 19
  val RS1_LSB = 15
  val RS2_MSB = 24
  val RS2_LSB = 20
  val RS3_MSB = 31
  val RS3_LSB = 27
}

// For fused-lui-load
case class Imm_LUI_LOAD() {
  def immFromLuiLoad(lui_imm: UInt, load_imm: UInt): UInt = {
    val loadImm = load_imm(Imm_I().len - 1, 0)
    Cat(lui_imm(Imm_U().len - loadImm.getWidth - 1, 0), loadImm)
  }
  def getLuiImm(uop: MicroOp): UInt = {
    val loadImmLen = Imm_I().len
    val imm_u = Cat(uop.psrc(1), uop.psrc(0), uop.ctrl.imm(ImmUnion.maxLen - 1, loadImmLen))
    Imm_U().do_toImm32(imm_u)
  }
}

class DecodeUnitIO(implicit p: Parameters) extends WXModule {
  val enq = new Bundle { val inst_packet = Input(new InstPacket) }
  val deq = new Bundle { val inst_ctrl_packet = Output(new InstCtrlPacket) }
  val csrCtrl = Input(new CustomCSRCtrlIO)
}

class DecodeUnit(implicit p: Parameters) extends WXModule with RVDecodeConstants {
  val io = IO(new DecodeUnitIO)

  val inst_packet = Wire(new InstPacket)
  val inst_ctrl_packet = Wire(new InstCtrlPacket)

  inst_packet := io.enq.inst_packet
  val decode_table = XDecode.table ++
    FDecode.table ++
    FDivSqrtDecode.table ++
    X64Decode.table ++
    XSTrapDecode.table ++
    BDecode.table ++
    CBODecode.table ++
    SvinvalDecode.table
  // assertion for LUI: only LUI should be assigned `selImm === SelImm.IMM_U && fuType === FuType.alu`
  val luiMatch = (t: Seq[BitPat]) => t(3).value == FuType.alu.litValue && t.reverse.head.value == SelImm.IMM_U.litValue
  val luiTable = decode_table.filter(t => luiMatch(t._2)).map(_._1).distinct
  assert(luiTable.length == 1 && luiTable.head == LUI, "Conflicts: LUI is determined by FuType and SelImm in Dispatch")

  inst_ctrl_packet.instPacket := inst_packet
  val cs: CtrlSignals = Wire(new CtrlSignals()).decode(inst_packet.instr, decode_table)
  cs.singleStep := false.B
  cs.replayInst := false.B
  cs.debug_globalID := DontCare

  val fpDecoder = Module(new FPDecoder)
  fpDecoder.io.instr := inst_packet.instr
  cs.fpu := fpDecoder.io.fpCtrl

  // for move elimination
  val isMove = BitPat("b000000000000_?????_000_?????_0010011") === inst_packet.instr
  cs.isMove := isMove && inst_packet.instr(RD_MSB, RD_LSB) =/= 0.U

  // read src1~3 location
  cs.lsrc(0) := inst_packet.instr(RS1_MSB, RS1_LSB)
  cs.lsrc(1) := inst_packet.instr(RS2_MSB, RS2_LSB)
  cs.lsrc(2) := inst_packet.instr(RS3_MSB, RS3_LSB)
  // read dest location
  cs.ldest := inst_packet.instr(RD_MSB, RD_LSB)

  // Invalid instr fill in exception vector
  inst_ctrl_packet.instPacket.exceptionVec := io.enq.inst_packet.exceptionVec
  inst_ctrl_packet.instPacket.exceptionVec(illegalInstr) := cs.selImm === SelImm.INVALID_INSTR

  when(!io.csrCtrl.svinval_enable) {
    val base_ii = cs.selImm === SelImm.INVALID_INSTR
    val sinval = BitPat("b0001011_?????_?????_000_00000_1110011") === inst_packet.instr
    val w_inval = BitPat("b0001100_00000_00000_000_00000_1110011") === inst_packet.instr
    val inval_ir = BitPat("b0001100_00001_00000_000_00000_1110011") === inst_packet.instr
    val svinval_ii = sinval || w_inval || inval_ir
    inst_ctrl_packet.instPacket.exceptionVec(illegalInstr) := base_ii || svinval_ii
    cs.flushPipe := false.B
  }

  // fix frflags
  //                           fflags    zero csrrs rd    csr
  val isFrflags = BitPat("b000000000001_00000_010_?????_1110011") === inst_packet.instr
  when(cs.fuType === FuType.csr && isFrflags) {
    cs.blockBackward := false.B
  }

  cs.imm := LookupTree(cs.selImm, ImmUnion.immSelMap.map(
    x => {
      val minBits = x._2.minBitsFromInstr(inst_packet.instr)
      require(minBits.getWidth == x._2.len)
      x._1 -> minBits
    }
  ))

  inst_ctrl_packet.ctrl := cs
  io.deq.inst_ctrl_packet := inst_ctrl_packet

  //-------------------------------------------------------------
  // Debug Info
  XSDebug("in:  instr=%x pc=%x excepVec=%b crossPageIPFFix=%d\n",
    io.enq.inst_packet.instr, io.enq.inst_packet.pc, io.enq.inst_packet.exceptionVec.asUInt,
    io.enq.inst_packet.crossPageIPFFix)
  XSDebug("out: srcType(0)=%b srcType(1)=%b srcType(2)=%b lsrc(0)=%d lsrc(1)=%d lsrc(2)=%d ldest=%d fuType=%b fuOpType=%b\n",
    io.deq.inst_ctrl_packet.ctrl.srcType(0), io.deq.inst_ctrl_packet.ctrl.srcType(1), io.deq.inst_ctrl_packet.ctrl.srcType(2),
    io.deq.inst_ctrl_packet.ctrl.lsrc(0), io.deq.inst_ctrl_packet.ctrl.lsrc(1), io.deq.inst_ctrl_packet.ctrl.lsrc(2),
    io.deq.inst_ctrl_packet.ctrl.ldest, io.deq.inst_ctrl_packet.ctrl.fuType, io.deq.inst_ctrl_packet.ctrl.fuOpType)
  XSDebug("out: rfWen=%d fpWen=%d isXSTrap=%d noSpecExec=%d isBlocked=%d flushPipe=%d imm=%x\n",
    io.deq.inst_ctrl_packet.ctrl.rfWen, io.deq.inst_ctrl_packet.ctrl.fpWen, io.deq.inst_ctrl_packet.ctrl.isXSTrap,
    io.deq.inst_ctrl_packet.ctrl.noSpecExec, io.deq.inst_ctrl_packet.ctrl.blockBackward, io.deq.inst_ctrl_packet.ctrl.flushPipe,
    io.deq.inst_ctrl_packet.ctrl.imm)
  XSDebug("out: excepVec=%b\n", io.deq.inst_ctrl_packet.instPacket.exceptionVec.asUInt)
}
