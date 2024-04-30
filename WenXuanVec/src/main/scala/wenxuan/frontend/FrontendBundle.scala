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
import wenxuan.common.WXBundle
import org.chipsalliance.cde.config.Parameters
import wenxuan.frontend.icache.HasICacheParameters
import wenxuan.common._
import utility._

/** Ftq request to ICache Prefetch */
class PrefetchRequest(implicit p: Parameters) extends WXBundle {
  val target          = UInt(VAddrBits.W)
}

class FtqPrefechBundle(implicit p: Parameters) extends WXBundle {
  val req = DecoupledIO(new PrefetchRequest)
}

/** Ftq request to ICache fetch */
class FtqICacheInfo(implicit p: Parameters)extends WXBundle with HasICacheParameters{
  val startAddr           = UInt(VAddrBits.W)
  val nextlineStart       = UInt(VAddrBits.W)
  // the default fetchWidth is half of the blockBytes, so when startAddr is in the middle of the cacheline,
  //  crossCacheline need to be set, means that the next line request is valid
  def crossCacheline =  startAddr(blockOffBits - 1) === 1.U
  def fromFtqPcBundle(b: Ftq_RF_Components) = {
    this.startAddr := b.startAddr
    this.nextlineStart := b.nextLineAddr
    this
  }
}

class FtqToICacheRequestBundle(implicit p: Parameters)extends WXBundle with HasICacheParameters{
  val pcMemRead           = Vec(5, new FtqICacheInfo)
  val readValid           = Vec(5, Bool())
}

/************ BPU <> Ftq ************/
/** BPU resp prediction info to Ftq */
class BranchPredictionBundle(implicit p: Parameters) extends WXBundle
  with HasBPUConst with BPUUtils
{
  val pc    = Vec(numDup, UInt(VAddrBits.W))
  val valid = Vec(numDup, Bool())
  val hasRedirect  = Vec(numDup, Bool())
  val ftq_idx = new FtqPtr
}

class BranchPredictionResp(implicit p: Parameters) extends WXBundle with HasBPUConst {
  // val valids = Vec(3, Bool())
  val s1 = new BranchPredictionBundle
  val s2 = new BranchPredictionBundle
  val s3 = new BranchPredictionBundle

  val last_stage_meta = UInt(MaxMetaLength.W)
  val last_stage_spec_info = new Ftq_Redirect_SRAMEntry
  val last_stage_ftb_entry = new FTBEntry

  val topdown_info = new FrontendTopDownBundle

  def lastStage = s3
  def selectedResp ={
    val res =
      PriorityMux(Seq(
        ((s3.valid(3) && s3.hasRedirect(3)) -> s3),
        ((s2.valid(3) && s2.hasRedirect(3)) -> s2),
        (s1.valid(3) -> s1)
      ))
    res
  }
  def selectedRespIdxForFtq =
    PriorityMux(Seq(
      ((s3.valid(3) && s3.hasRedirect(3)) -> BP_S3),
      ((s2.valid(3) && s2.hasRedirect(3)) -> BP_S2),
      (s1.valid(3) -> BP_S1)
    ))
}

class BpuToFtqIO(implicit p: Parameters) extends WXBundle {
  val resp = DecoupledIO(new BranchPredictionResp())
}

/** Ftq misc information to BPU */

class BranchPredictionUpdate(implicit p: Parameters) extends WXBundle with HasBPUConst {
  val pc = UInt(VAddrBits.W)
  val spec_info = new SpeculativeInfo
  val ftb_entry = new FTBEntry()

  val cfi_idx = ValidUndirectioned(UInt(log2Ceil(PredictWidth).W))
  val br_taken_mask = Vec(numBr, Bool())
  val br_committed = Vec(numBr, Bool()) // High only when br valid && br committed
  val jmp_taken = Bool()
  val mispred_mask = Vec(numBr+1, Bool())
  val pred_hit = Bool()
  val false_hit = Bool()
  val new_br_insert_pos = Vec(numBr, Bool())
  val old_entry = Bool()
  val meta = UInt(MaxMetaLength.W)
  val full_target = UInt(VAddrBits.W)
  val from_stage = UInt(2.W)
  val ghist = UInt(HistoryLength.W)

  def is_jal = ftb_entry.tailSlot.valid && ftb_entry.isJal
  def is_jalr = ftb_entry.tailSlot.valid && ftb_entry.isJalr
  def is_call = ftb_entry.tailSlot.valid && ftb_entry.isCall
  def is_ret = ftb_entry.tailSlot.valid && ftb_entry.isRet

  def is_call_taken = is_call && jmp_taken && cfi_idx.valid && cfi_idx.bits === ftb_entry.tailSlot.offset
  def is_ret_taken = is_ret && jmp_taken && cfi_idx.valid && cfi_idx.bits === ftb_entry.tailSlot.offset

  def display(cond: Bool) = {
    XSDebug(cond, p"-----------BranchPredictionUpdate-----------\n")
    XSDebug(cond, p"[mispred_mask] ${Binary(mispred_mask.asUInt)} [false_hit] $false_hit\n")
    XSDebug(cond, p"[new_br_insert_pos] ${Binary(new_br_insert_pos.asUInt)}\n")
    XSDebug(cond, p"--------------------------------------------\n")
  }
}

class BranchPredictionRedirect(implicit p: Parameters) extends Redirect with HasBPUConst {
  // TODO: backend should pass topdown signals here
  // must not change its parent since BPU has used asTypeOf(this type) from its parent class
  require(isInstanceOf[Redirect])
  val BTBMissBubble = Bool()
  def ControlRedirectBubble = debugIsCtrl
  // if mispred br not in ftb, count as BTB miss
  def ControlBTBMissBubble = ControlRedirectBubble && !cfiUpdate.br_hit && !cfiUpdate.jr_hit
  def TAGEMissBubble = ControlRedirectBubble && cfiUpdate.br_hit && !cfiUpdate.sc_hit
  def SCMissBubble = ControlRedirectBubble && cfiUpdate.br_hit && cfiUpdate.sc_hit
  def ITTAGEMissBubble = ControlRedirectBubble && cfiUpdate.jr_hit && !cfiUpdate.pd.isRet
  def RASMissBubble = ControlRedirectBubble && cfiUpdate.jr_hit && cfiUpdate.pd.isRet
  def MemVioRedirectBubble = debugIsMemVio
  def OtherRedirectBubble = !debugIsCtrl && !debugIsMemVio

  def connectRedirect(source: Redirect): Unit = {
    for ((name, data) <- this.elements) {
      if (source.elements.contains(name)) {
        data := source.elements(name)
      }
    }
  }

  def display(cond: Bool): Unit = {
    XSDebug(cond, p"-----------BranchPredictionRedirect----------- \n")
    XSDebug(cond, p"-----------cfiUpdate----------- \n")
    XSDebug(cond, p"[pc] ${Hexadecimal(cfiUpdate.pc)}\n")
    // XSDebug(cond, p"[hist] ${Binary(cfiUpdate.hist.predHist)}\n")
    XSDebug(cond, p"[br_hit] ${cfiUpdate.br_hit} [isMisPred] ${cfiUpdate.isMisPred}\n")
    XSDebug(cond, p"[pred_taken] ${cfiUpdate.predTaken} [taken] ${cfiUpdate.taken} [isMisPred] ${cfiUpdate.isMisPred}\n")
    XSDebug(cond, p"[target] ${Hexadecimal(cfiUpdate.target)} \n")
    XSDebug(cond, p"[shift] ${cfiUpdate.shift}\n")
    XSDebug(cond, p"------------------------------- \n")
    XSDebug(cond, p"[robPtr] f=${robIdx.flag} v=${robIdx.value}\n")
    XSDebug(cond, p"[ftqPtr] f=${ftqIdx.flag} v=${ftqIdx.value} \n")
    XSDebug(cond, p"[ftqOffset] ${ftqOffset} \n")
    XSDebug(cond, p"[stFtqIdx] f=${stFtqIdx.flag} v=${stFtqIdx.value}\n")
    XSDebug(cond, p"[stFtqOffset] ${stFtqOffset}\n")
    XSDebug(cond, p"---------------------------------------------- \n")
  }
}
class FtqToBpuIO(implicit p: Parameters) extends WXBundle {
  val redirect = Valid(new BranchPredictionRedirect)
  val update = Valid(new BranchPredictionUpdate)
  val enq_ptr = Output(new FtqPtr)
}

/************ IFU <> Ftq ************/
/** FTQ req, redirect, flush ... informaiton to IFU */
class FetchRequestBundle(implicit p: Parameters) extends WXBundle with HasICacheParameters {
  //fast path: Timing critical
  val startAddr = UInt(VAddrBits.W)
  val nextlineStart = UInt(VAddrBits.W)
  val nextStartAddr = UInt(VAddrBits.W)
  //slow path
  val ftqIdx = new FtqPtr
  val ftqOffset = ValidUndirectioned(UInt(log2Ceil(PredictWidth).W))

  def fromFtqPcBundle(b: Ftq_RF_Components) = {
    this.startAddr := b.startAddr
    this.nextlineStart := b.nextLineAddr
    when(b.fallThruError) {
      val nextBlockHigherTemp = Mux(startAddr(log2Ceil(PredictWidth) + instOffsetBits), b.startAddr, b.nextLineAddr)
      val nextBlockHigher = nextBlockHigherTemp(VAddrBits - 1, log2Ceil(PredictWidth) + instOffsetBits + 1)
      this.nextStartAddr :=
        Cat(nextBlockHigher,
          startAddr(log2Ceil(PredictWidth) + instOffsetBits) ^ 1.U(1.W),
          startAddr(log2Ceil(PredictWidth) + instOffsetBits - 1, instOffsetBits),
          0.U(instOffsetBits.W)
        )
    }
    this
  }
}

class FtqToIfuIO(implicit p: Parameters) extends WXBundle with HasCircularQueuePtrHelper {
  val req = Decoupled(new FetchRequestBundle)
  val redirect = Valid(new BranchPredictionRedirect)
  val topdown_redirect = Valid(new BranchPredictionRedirect)
  val flushFromBpu = new Bundle {
    // when ifu pipeline is not stalled,
    // a packet from bpu s3 can reach f1 at most
    val s2 = Valid(new FtqPtr)
    val s3 = Valid(new FtqPtr)
    def shouldFlushBy(src: Valid[FtqPtr], idx_to_flush: FtqPtr) = {
      src.valid && !isAfter(src.bits, idx_to_flush)
    }
    def shouldFlushByStage2(idx: FtqPtr) = shouldFlushBy(s2, idx)
    def shouldFlushByStage3(idx: FtqPtr) = shouldFlushBy(s3, idx)
  }
}