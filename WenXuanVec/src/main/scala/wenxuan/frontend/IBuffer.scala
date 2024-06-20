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

package wenxuan.frontend

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility.HasCircularQueuePtrHelper
import utils.HasPerfEvents
import wenxuan.common.{InstPacket, WXBundle, WXModule}

class IBufEntry(implicit p: Parameters) extends WXBundle{
  val inst = UInt(32.W)
  val pc = UInt(VAddrBits.W)
  val foldpc = UInt(MemPredPCWidth.W)
  val pd = new PreDecodeInfo
  val pred_taken = Bool()
  val ftqPtr = new FtqPtr
  val ftqOffset = UInt(log2Ceil(PredictWidth).W)
  val ipf = Bool()
  val acf = Bool()
  val crossPageIPFFix = Bool()
  val triggered = new TriggerCf

  def fromFetch(fetch: FetchToIBuffer, i: Int): IBufEntry = {
    inst   := fetch.instrs(i)
    pc     := fetch.pc(i)
    foldpc := fetch.foldpc(i)
    pd     := fetch.pd(i)
    pred_taken := fetch.ftqOffset(i).valid
    ftqPtr := fetch.ftqPtr
    ftqOffset := fetch.ftqOffset(i).bits
    ipf := fetch.ipf(i)
    acf := fetch.acf(i)
    crossPageIPFFix := fetch.crossPageIPFFix(i)
    triggered := fetch.triggered(i)
    this
  }

  def toInstPacket: InstPacket = {
    val inst_p = Wire(new InstPacket)
    inst_p.instr := inst
    inst_p.pc := pc
    inst_p.foldpc := foldpc
    inst_p.exceptionVec := 0.U.asTypeOf(ExceptionVec())
    inst_p.exceptionVec(instrPageFault) := ipf
    inst_p.exceptionVec(instrAccessFault) := acf
    inst_p.trigger := triggered
    inst_p.pd := pd
    inst_p.pred_taken := pred_taken
    inst_p.crossPageIPFFix := crossPageIPFFix
    inst_p.storeSetHit := DontCare
    inst_p.waitForRobIdx := DontCare
    inst_p.loadWaitBit := DontCare
    inst_p.loadWaitStrict := DontCare
    inst_p.ssid := DontCare
    inst_p.ftqPtr := ftqPtr
    inst_p.ftqOffset := ftqOffset
    inst_p
  }
}

class IBufferIO(implicit p: Parameters) extends WXBundle {
  val flush = Input(Bool())
  val ControlRedirect = Input(Bool())
  val ControlBTBMissBubble = Input(Bool())
  val TAGEMissBubble = Input(Bool())
  val SCMissBubble = Input(Bool())
  val ITTAGEMissBubble = Input(Bool())
  val RASMissBubble = Input(Bool())
  val MemVioRedirect = Input(Bool())
  val in = Flipped(DecoupledIO(new FetchToIBuffer))
  val out = Vec(DecodeWidth, DecoupledIO(new InstPacket))
  val full = Output(Bool())
  val stallReason = new StallReasonIO(DecodeWidth)
}

class IBuffer(implicit p: Parameters) extends WXModule
  with HasCircularQueuePtrHelper with HasPerfEvents {
  val io = IO(new IBufferIO)

  val perfEvents = Seq(
    ("IBuffer_Flushed  ", io.flush),
    ("IBuffer_hungry   ", instrHungry),
    ("IBuffer_1_4_valid", (validEntries > (0 * (IBufSize / 4)).U) & (validEntries < (1 * (IBufSize / 4)).U)),
    ("IBuffer_2_4_valid", (validEntries >= (1 * (IBufSize / 4)).U) & (validEntries < (2 * (IBufSize / 4)).U)),
    ("IBuffer_3_4_valid", (validEntries >= (2 * (IBufSize / 4)).U) & (validEntries < (3 * (IBufSize / 4)).U)),
    ("IBuffer_4_4_valid", (validEntries >= (3 * (IBufSize / 4)).U) & (validEntries < (4 * (IBufSize / 4)).U)),
    ("IBuffer_full     ", validEntries.andR),
    ("Front_Bubble     ", PopCount((0 until DecodeWidth).map(i => io.out(i).ready && !io.out(i).valid)))
  )
  generatePerfEvent()
}
