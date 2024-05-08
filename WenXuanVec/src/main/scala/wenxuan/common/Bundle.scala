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

package wenxuan.common

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import wenxuan.cache.HasDCacheParameters
import wenxuan.frontend._
import wenxuan.backend.rob.RobPtr
import utility._
import wenxuan.commonType._
import wenxuan.backendInfoType._

class ValidUndirectioned[T <: Data](gen: T) extends Bundle {
  val valid = Bool()
  val bits = gen.cloneType.asInstanceOf[T]

}

object ValidUndirectioned {
  def apply[T <: Data](gen: T) = {
    new ValidUndirectioned[T](gen)
  }
}

/** Distribute to CSR signal bundle */
class DistributedCSRIO(implicit p: Parameters) extends WXBundle {
  // CSR has been written by csr inst, copies of csr should be updated
  val w = ValidIO(new Bundle {
    val addr = Output(UInt(12.W))
    val data = Output(UInt(XLEN.W))
  })
}


class DistributedCSRUpdateReq(implicit p: Parameters) extends WXBundle {
  // Request csr to be updated
  //
  // Note that this request will ONLY update CSR Module it self,
  // copies of csr will NOT be updated, use it with care!
  //
  // For each cycle, no more than 1 DistributedCSRUpdateReq is valid
  val w = ValidIO(new Bundle {
    val addr = Output(UInt(12.W))
    val data = Output(UInt(XLEN.W))
  })
  def apply(valid: Bool, addr: UInt, data: UInt, src_description: String) = {
    when(valid){
      w.bits.addr := addr
      w.bits.data := data
    }
    println("Distributed CSR update req registered for " + src_description)
  }
}

/** custom l2 - l1 interface */
class L2ToL1Hint(implicit p: Parameters) extends WXBundle with HasDCacheParameters {
  val sourceId = UInt(log2Up(cacheParams.nMissEntries).W)    // tilelink sourceID -> mshr id
  val isKeyword = Bool()                             // miss entry keyword -> L1 load queue replay
}


/** satp csr */
class SatpStruct(implicit p: Parameters) extends WXBundle {
  val mode = UInt(4.W) // virtual memory system mode, sv32/sv39/sv48 ...
  val asid = UInt(16.W)
  val ppn  = UInt(44.W)
}

class TlbSatpBundle(implicit p: Parameters) extends SatpStruct {
  val changed = Bool()

  def apply(satp_value: UInt): Unit = {
    require(satp_value.getWidth == XLEN)
    val sa = satp_value.asTypeOf(new SatpStruct)
    mode := sa.mode
    asid := sa.asid
    ppn := Cat(0.U((44-PAddrBits).W), sa.ppn(PAddrBits-1, 0)).asUInt
    changed := DataChanged(sa.asid) // when ppn is changed, software need do the flush
  }
}

class TlbCsrBundle(implicit p: Parameters) extends WXBundle {
  val satp = new TlbSatpBundle() // satp csr
  val priv = new Bundle{
    val mxr = Bool() // mxr field in mstatus csr, takes effect in privilege of load access
    val sum = Bool() // sum field in mstatus csr, takes effect in privilege of load & store access
    val imode = UInt(2.W) // privilege in instruction mode
    val dmode = UInt(2.W) // privilege in debug mode
  }
}

/** sfence  */
class SfenceBundle(implicit p: Parameters) extends WXBundle {
  val valid = Bool()
  val bits = new Bundle {
    val rs1 = Bool()
    val rs2 = Bool()
    val addr = UInt(VAddrBits.W)
    val asid = UInt(AsidLen.W)
    val flushPipe = Bool()
  }

  override def toPrintable: Printable = {
    p"valid:0x${Hexadecimal(valid)} rs1:${bits.rs1} rs2:${bits.rs2} addr:${Hexadecimal(bits.addr)}, flushPipe:${bits.flushPipe}"
  }
}

// cfi update. cfi means call frame instr/info, including br/jmp/call/ret
class CfiUpdateInfo(implicit p: Parameters) extends WXBundle with HasBPUParameter with HasBPUConst {
  // from backend
  val pc = UInt(VAddrBits.W)
  // frontend -> backend -> frontend
  val pd = new PreDecodeInfo
  val ssp = UInt(log2Up(RasSize).W)
  val sctr = UInt(log2Up(RasCtrSize).W)
  val TOSW = new RASPtr
  val TOSR = new RASPtr
  val NOS = new RASPtr
  val topAddr = UInt(VAddrBits.W)
  // val hist = new ShiftingGlobalHistory
  val folded_hist = new AllFoldedHistories(foldedGHistInfos)
  val afhob = new AllAheadFoldedHistoryOldestBits(foldedGHistInfos)
  val lastBrNumOH = UInt((numBr+1).W)
  val ghr = UInt(UbtbGHRLength.W)
  val histPtr = new CGHPtr
  val specCnt = Vec(numBr, UInt(10.W))
  // need pipeline update
  val br_hit = Bool() // if in ftb entry
  val jr_hit = Bool() // if in ftb entry
  val sc_hit = Bool() // if used in ftb entry, invalid if !br_hit
  val predTaken = Bool()
  val target = UInt(VAddrBits.W)
  val taken = Bool()
  val isMisPred = Bool()
  val shift = UInt((log2Ceil(numBr)+1).W)
  val addIntoHist = Bool()

  def fromFtqRedirectSram(entry: Ftq_Redirect_SRAMEntry) = {
    // this.hist := entry.ghist
    this.folded_hist := entry.folded_hist
    this.lastBrNumOH := entry.lastBrNumOH
    this.afhob := entry.afhob
    this.histPtr := entry.histPtr
    this.ssp := entry.ssp
    this.sctr := entry.sctr
    this.TOSW := entry.TOSW
    this.TOSR := entry.TOSR
    this.NOS := entry.NOS
    this.topAddr := entry.topAddr
    this
  }
}

class Redirect(implicit p: Parameters) extends WXBundle {
  val isRVC = Bool()
  val robIdx = new RobPtr
  val ftqIdx = new FtqPtr
  val ftqOffset = UInt(log2Up(PredictWidth).W)
  val level = RedirectLevel()
  val interrupt = Bool()
  val cfiUpdate = new CfiUpdateInfo

  val stFtqIdx = new FtqPtr // for load violation predict
  val stFtqOffset = UInt(log2Up(PredictWidth).W)

  val debug_runahead_checkpoint_id = UInt(64.W)
  val debugIsCtrl = Bool()
  val debugIsMemVio = Bool()

  // def isUnconditional() = RedirectLevel.isUnconditional(level)
  def flushItself() = RedirectLevel.flushItself(level)
  // def isException() = RedirectLevel.isException(level)
}

/** frontend <> backend */
// backend ROB commit info
class RobCommitInfo(implicit p: Parameters) extends WXBundle {
  val ldest = UInt(5.W)
  val rfWen = Bool()
  val fpWen = Bool()
  val wflags = Bool()
  val commitType = CommitType()
  val pdest = UInt(PhyRegIdxWidth.W)
  val ftqIdx = new FtqPtr
  val ftqOffset = UInt(log2Up(PredictWidth).W)
  val isMove = Bool()
  val isRVC = Bool()
  // these should be optimized for synthesis verilog
  val pc = UInt(VAddrBits.W)
}

// ftq to backend ctrlblock
class FtqToCtrlIO(implicit p: Parameters) extends WXBundle with HasBackendRedirectInfo {
  // write to backend pc mem
  val pc_mem_wen = Output(Bool())
  val pc_mem_waddr = Output(UInt(log2Ceil(FtqSize).W))
  val pc_mem_wdata = Output(new Ftq_RF_Components)
  // newest target
  val newest_entry_target = Output(UInt(VAddrBits.W))
  val newest_entry_ptr = Output(new FtqPtr)
}
// backend ctrlblock to ftq
class CtrlToFtqIO(implicit p: Parameters) extends WXBundle {
  val rob_commits = Vec(CommitWidth, Valid(new RobCommitInfo))
  val redirect = Valid(new Redirect)
  val ftqIdxAhead = Vec(BackendRedirectNum, Valid(new FtqPtr))
  val ftqIdxSelOH = Valid(UInt((BackendRedirectNum).W))
}
// frontend <> bakcend ctrlblock
class FrontendToCtrlIO(implicit p: Parameters) extends WXBundle {
  // to backend end
  val cfVec = Vec(DecodeWidth, DecoupledIO(new CtrlFlow))
  val stallReason = new StallReasonIO(DecodeWidth)
  val fromFtq = new FtqToCtrlIO
  // from backend
  val toFtq = Flipped(new CtrlToFtqIO)
}
