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

package wenxuan.backend

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import wenxuan.common.{InstPacket, RobCommitInfo, WXBundle}
import wenxuan.commonType.{CommitType, FuType, SelImm, SrcState, SrcType}
import wenxuan.backend.decode.{ImmUnion, XDecode}
import wenxuan.backend.rob.RobPtr
import wenxuan.functionOpType._

class FPUCtrlSignals(implicit p: Parameters) extends WXBundle {
  val isAddSub = Bool() // swap23
  val typeTagIn = UInt(1.W)
  val typeTagOut = UInt(1.W)
  val fromInt = Bool()
  val wflags = Bool()
  val fpWen = Bool()
  val fmaCmd = UInt(2.W)
  val div = Bool()
  val sqrt = Bool()
  val fcvt = Bool()
  val typ = UInt(2.W)
  val fmt = UInt(2.W)
  val ren3 = Bool() //TODO: remove SrcType.fp
  val rm = UInt(3.W)
}

// decoder output control signal
class CtrlSignals(implicit p: Parameters) extends WXBundle {
  val debug_globalID = UInt(XLEN.W)
  val srcType = Vec(3, SrcType())
  val lsrc = Vec(3, UInt(5.W))
  val ldest = UInt(5.W)
  val fuType = FuType()
  val fuOpType = FuOpType()
  val rfWen = Bool()
  val fpWen = Bool()
  val isXSTrap = Bool()
  val noSpecExec = Bool() // wait forward
  val blockBackward = Bool() // block backward
  val flushPipe = Bool() // This inst will flush all the pipe when commit, like exception but can commit
  val selImm = SelImm()
  val imm = UInt(ImmUnion.maxLen.W)
  val commitType = CommitType()
  val fpu = new FPUCtrlSignals
  val isMove = Bool() // for move elimination
  val singleStep = Bool() // single Step Module from CSR ctrl info
  // This inst will flush all the pipe when it is the oldest inst in ROB,
  // then replay from this inst itself
  val replayInst = Bool()

  private def allSignals = srcType ++ Seq(fuType, fuOpType, rfWen, fpWen,
    isXSTrap, noSpecExec, blockBackward, flushPipe, selImm)

  def decode(inst: UInt, table: Iterable[(BitPat, List[BitPat])]): CtrlSignals = {
    val decoder = freechips.rocketchip.rocket.DecodeLogic(inst, XDecode.decodeDefault, table)
    allSignals zip decoder foreach { case (s, d) => s := d }
    commitType := DontCare
    this
  }

  def isSoftPrefetch: Bool = {
    fuType === FuType.alu && fuOpType === ALUOpType.or && selImm === SelImm.IMM_I && ldest === 0.U
  }
}
// Decoder output Ctrl Signal Bundle
class InstCtrlPacket(implicit p: Parameters) extends WXBundle {
  val instPacket = new InstPacket
  val ctrl = new CtrlSignals
}
// Rename output MicroOp (uop)
class MicroOp(implicit p: Parameters) extends InstCtrlPacket {
  val robIdx = new RobPtr // rob idx
  val srcState = Vec(3, SrcState()) // busy or ready
  val psrc = Vec(3, UInt(PhyRegIdxWidth.W))
  val pdest = UInt(PhyRegIdxWidth.W)
  val eliminatedMove = Bool() // move instr
  val lqIdx = new LqPtr
  val sqIdx = new SqPtr
  val snapshot = Bool()
  val debugInfo = new PerfDebugInfo
}

// Snapshot info for backup snapshot and resume for arch resource
class SnapshotPort(implicit p: Parameters) extends WXBundle {
  val snptEnq = Bool()
  val snptDeq = Bool()
  val useSnpt = Bool()
  val snptSelect = UInt(log2Ceil(RenameSnapshotNum).W)
}

// CSR control to all core components
class CustomCSRCtrlIO(implicit p: Parameters) extends WXBundle {
  // Decode: check svinval at decode and make instr invalid if svinval_enable is false
  val svinval_enable = Bool()
  // Rename
  val fusion_enable = Bool()
  val singlestep = Bool()

}

// ROB commit & WALK
class RobCommitIO(implicit p: Parameters) extends WXBundle {
  // instr packet commit
  val isCommit = Bool()
  val commitValid = Vec(CommitWidth, Bool())
  // ROB WALK resume
  val isWalk = Bool()
  val walkValid = Vec(CommitWidth, Bool())
  // retire
  val robIdx = Vec(CommitWidth, new RobPtr)
  // ROB info
  val info = Vec(CommitWidth, new RobCommitInfo)
}

// Redirect info
class Redirect(implicit p: Parameters) extends WXBundle {
  val robIdx = new RobPtr() // redirect instr robIdx, used to resume (snapshot and WALK)
}

class PerfDebugInfo(implicit p: Parameters) extends WXBundle {
  val eliminatedMove = Bool()
  // val fetchTime = UInt(XLEN.W)
  val renameTime = UInt(XLEN.W)
  val dispatchTime = UInt(XLEN.W)
  val enqRsTime = UInt(XLEN.W)
  val selectTime = UInt(XLEN.W)
  val issueTime = UInt(XLEN.W)
  val writebackTime = UInt(XLEN.W)
  // val commitTime = UInt(XLEN.W)
  val runahead_checkpoint_id = UInt(XLEN.W)
  val tlbFirstReqTime = UInt(XLEN.W)
  val tlbRespTime = UInt(XLEN.W) // when getting hit result (including delay in L2TLB hit)
}