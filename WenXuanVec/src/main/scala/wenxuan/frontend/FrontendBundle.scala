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
import wenxuan.frontend.icache.HasICacheParameters
import wenxuan.common._
import wenxuan.commonType._
import utility._

class FrontendTopDownBundle(implicit p: Parameters) extends WXBundle {
  val reasons = Vec(TopDownCounters.NumStallReasons.id, Bool())
  val stallWidth = UInt(log2Ceil(PredictWidth).W)
}

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
  // the default fetchblockBytes is half of the blockBytes, so if startAddr is over the middle of the cacheline,
  //  crossCacheline need to be set, means that the next line request is needed
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

/************ Global History matters ************/
abstract class GlobalHistory(implicit p: Parameters) extends WXBundle with HasBPUConst {
  def update(br_valids: Vec[Bool], real_taken_mask: Vec[Bool]): GlobalHistory
}

class ShiftingGlobalHistory(implicit p: Parameters) extends GlobalHistory {
  val predHist = UInt(HistoryLength.W)

  def update(shift: UInt, taken: Bool, hist: UInt = this.predHist): ShiftingGlobalHistory = {
    val g = Wire(new ShiftingGlobalHistory)
    g.predHist := (hist << shift) | taken
    g
  }

  def update(br_valids: Vec[Bool], real_taken_mask: Vec[Bool]): ShiftingGlobalHistory = {
    require(br_valids.length == numBr)
    require(real_taken_mask.length == numBr)
    val last_valid_idx = PriorityMux(
      br_valids.reverse :+ true.B,
      (numBr to 0 by -1).map(_.U(log2Ceil(numBr+1).W))
    )
    val first_taken_idx = PriorityEncoder(false.B +: real_taken_mask)
    val smaller = Mux(last_valid_idx < first_taken_idx,
      last_valid_idx,
      first_taken_idx
    )
    val shift = smaller
    val taken = real_taken_mask.reduce(_||_)
    update(shift, taken, this.predHist)
  }

  // static read
  def read(n: Int): Bool = predHist.asBools(n)

  final def === (that: ShiftingGlobalHistory): Bool = {
    predHist === that.predHist
  }

  final def =/= (that: ShiftingGlobalHistory): Bool = !(this === that)
}

// circular global history pointer
class CGHPtr(implicit p: Parameters) extends CircularQueuePtr[CGHPtr](
  p => p(WXVTileKey).core.HistoryLength
){
}

object CGHPtr {
  def apply(f: Bool, v: UInt)(implicit p: Parameters): CGHPtr = {
    val ptr = Wire(new CGHPtr)
    ptr.flag := f
    ptr.value := v
    ptr
  }
  def inverse(ptr: CGHPtr)(implicit p: Parameters): CGHPtr = {
    apply(!ptr.flag, ptr.value)
  }
}

class CircularGlobalHistory(implicit p: Parameters) extends GlobalHistory {
  val buffer = Vec(HistoryLength, Bool())
  type HistPtr = UInt
  def update(br_valids: Vec[Bool], real_taken_mask: Vec[Bool]): CircularGlobalHistory = {
    this
  }
}
class AheadFoldedHistoryOldestBits(val len: Int, val max_update_num: Int)(implicit p: Parameters) extends WXBundle with HasBPUConst{
  val bits = Vec(max_update_num*2, Bool())
  // def info = (len, compLen)
  def getRealOb(brNumOH: UInt): Vec[Bool] = {
    val ob = Wire(Vec(max_update_num, Bool()))
    for (i <- 0 until max_update_num) {
      ob(i) := Mux1H(brNumOH, bits.drop(i).take(numBr+1))
    }
    ob
  }
}
class AllAheadFoldedHistoryOldestBits(val gen: Seq[Tuple2[Int, Int]])(implicit p: Parameters) extends WXBundle with HasBPUConst {
  val afhob = MixedVec(gen.filter(t => t._1 > t._2).map{_._1}
    .toSet.toList.map(l => new AheadFoldedHistoryOldestBits(l, numBr))) // remove duplicates
  require(gen.toSet.toList.equals(gen))
  def getObWithInfo(info: Tuple2[Int, Int]) = {
    val selected = afhob.filter(_.len == info._1)
    require(selected.length == 1)
    selected(0)
  }
  def read(ghv: Vec[Bool], ptr: CGHPtr) = {
    val hisLens = afhob.map(_.len)
    val bitsToRead = hisLens.flatMap(l => (0 until numBr*2).map(i => l-i-1)).toSet // remove duplicates
    val bitsWithInfo = bitsToRead.map(pos => (pos, ghv((ptr+(pos+1).U).value)))
    for (ob <- afhob) {
      for (i <- 0 until numBr*2) {
        val pos = ob.len - i - 1
        val bit_found = bitsWithInfo.filter(_._1 == pos).toList
        require(bit_found.length == 1)
        ob.bits(i) := bit_found(0)._2
      }
    }
  }
}
/************ BPU Prediction Bundle ************/
/** BasicPrediction used by all BPU */
trait BasicPrediction extends HasWXCommonParameters {
  def cfiIndex: ValidUndirectioned[UInt]
  def target(pc: UInt): UInt
  def lastBrPosOH: Vec[Bool]
  def brTaken: Bool
  def shouldShiftVec: Vec[Bool]
  def fallThruError: Bool
}

// selectByTaken selects some data according to takenMask
// allTargets should be in a Vec, like [taken0, taken1, ..., not taken, not hit]
object selectByTaken {
  def apply[T <: Data](takenMask: Vec[Bool], hit: Bool, allTargets: Vec[T]): T = {
    val selVecOH =
      takenMask.zipWithIndex.map { case (t, i) => !takenMask.take(i).fold(false.B)(_ || _) && t && hit } :+
        (!takenMask.asUInt.orR && hit) :+ !hit
    Mux1H(selVecOH, allTargets)
  }
}

/** Full Branch Prediction */
class FullBranchPrediction(implicit p: Parameters) extends WXBundle with HasBPUConst with BasicPrediction {
  val br_taken_mask = Vec(numBr, Bool())

  val slot_valids = Vec(totalSlot, Bool())

  val targets = Vec(totalSlot, UInt(VAddrBits.W))
  val jalr_target = UInt(VAddrBits.W) // special path for indirect predictors
  val offsets = Vec(totalSlot, UInt(log2Ceil(PredictWidth).W))
  val fallThroughAddr = UInt(VAddrBits.W)
  val fallThroughErr = Bool()

  val is_jal = Bool()
  val is_jalr = Bool()
  val is_call = Bool()
  val is_ret = Bool()
  val last_may_be_rvi_call = Bool()
  val is_br_sharing = Bool()

  // val call_is_rvc = Bool()
  val hit = Bool()

  val predCycle = if (!env.FPGAPlatform) Some(UInt(64.W)) else None

  def br_slot_valids = slot_valids.init
  def tail_slot_valid = slot_valids.last

  def br_valids = {
    VecInit(br_slot_valids :+ (tail_slot_valid && is_br_sharing))
  }

  def taken_mask_on_slot = {
    VecInit(
      (br_slot_valids zip br_taken_mask.init).map{ case (t, v) => t && v } :+ (
        tail_slot_valid && (
          is_br_sharing && br_taken_mask.last || !is_br_sharing
          )
        )
    )
  }

  def real_slot_taken_mask(): Vec[Bool] = {
    VecInit(taken_mask_on_slot.map(_ && hit))
  }

  // len numBr
  def real_br_taken_mask(): Vec[Bool] = {
    VecInit(
      taken_mask_on_slot.map(_ && hit).init :+
        (br_taken_mask.last && tail_slot_valid && is_br_sharing && hit)
    )
  }

  // the vec indicating if ghr should shift on each branch
  def shouldShiftVec =
    VecInit(br_valids.zipWithIndex.map{ case (v, i) =>
      v && !real_br_taken_mask.take(i).reduceOption(_||_).getOrElse(false.B)})

  def lastBrPosOH =
    VecInit((!hit || !br_valids.reduce(_||_)) +: // not hit or no brs in entry
      (0 until numBr).map(i =>
        br_valids(i) &&
          !real_br_taken_mask.take(i).reduceOption(_||_).getOrElse(false.B) && // no brs taken in front it
          (real_br_taken_mask()(i) || !br_valids.drop(i+1).reduceOption(_||_).getOrElse(false.B)) && // no brs behind it
          hit
      )
    )

  def brTaken = (br_valids zip br_taken_mask).map{ case (a, b) => a && b && hit}.reduce(_||_)

  def target(pc: UInt): UInt = {
    selectByTaken(taken_mask_on_slot, hit, allTarget(pc))
  }

  // allTarget return a Vec of all possible target of a BP stage
  // in the following order: [taken_target0, taken_target1, ..., fallThroughAddr, not hit (plus fetch width)]
  //
  // This exposes internal targets for timing optimization,
  // since usually targets are generated quicker than taken
  def allTarget(pc: UInt): Vec[UInt] = {
    VecInit(targets :+ fallThroughAddr :+ (pc + (FetchWidth * 4).U))
  }

  def fallThruError: Bool = hit && fallThroughErr

  def hit_taken_on_jmp =
    !real_slot_taken_mask().init.reduce(_||_) &&
      real_slot_taken_mask().last && !is_br_sharing
  def hit_taken_on_call = hit_taken_on_jmp && is_call
  def hit_taken_on_ret  = hit_taken_on_jmp && is_ret
  def hit_taken_on_jalr = hit_taken_on_jmp && is_jalr

  def cfiIndex = {
    val cfiIndex = Wire(ValidUndirectioned(UInt(log2Ceil(PredictWidth).W)))
    cfiIndex.valid := real_slot_taken_mask().asUInt.orR
    // when no takens, set cfiIndex to PredictWidth-1
    cfiIndex.bits :=
      ParallelPriorityMux(real_slot_taken_mask(), offsets) |
        Fill(log2Ceil(PredictWidth), (!real_slot_taken_mask().asUInt.orR).asUInt)
    cfiIndex
  }

  def taken = br_taken_mask.reduce(_||_) || slot_valids.last // || (is_jal || is_jalr)

  def fromFtbEntry(
                    entry: FTBEntry,
                    pc: UInt,
                    last_stage_pc: Option[Tuple2[UInt, Bool]] = None,
                    last_stage_entry: Option[Tuple2[FTBEntry, Bool]] = None
                  ) = {
    slot_valids := entry.brSlots.map(_.valid) :+ entry.tailSlot.valid
    targets := entry.getTargetVec(pc, last_stage_pc) // Use previous stage pc for better timing
    jalr_target := targets.last
    offsets := entry.getOffsetVec
    is_jal := entry.tailSlot.valid && entry.isJal
    is_jalr := entry.tailSlot.valid && entry.isJalr
    is_call := entry.tailSlot.valid && entry.isCall
    is_ret := entry.tailSlot.valid && entry.isRet
    last_may_be_rvi_call := entry.last_may_be_rvi_call
    is_br_sharing := entry.tailSlot.valid && entry.tailSlot.sharing
    predCycle.map(_ := GTimer())

    val startLower        = Cat(0.U(1.W),    pc(instOffsetBits+log2Ceil(PredictWidth)-1, instOffsetBits))
    val endLowerwithCarry = Cat(entry.carry, entry.pftAddr)
    fallThroughErr := startLower >= endLowerwithCarry
    fallThroughAddr := Mux(fallThroughErr, pc + (FetchWidth * 4).U, entry.getFallThrough(pc, last_stage_entry))
  }

  def display(cond: Bool): Unit = {
    XSDebug(cond, p"[taken_mask] ${Binary(br_taken_mask.asUInt)} [hit] $hit\n")
  }
}

class BranchPredictionBundle(implicit p: Parameters) extends WXBundle
  with HasBPUConst with BPUUtils
{
  val pc    = Vec(numDup, UInt(VAddrBits.W))
  val valid = Vec(numDup, Bool())
  val hasRedirect  = Vec(numDup, Bool())
  val ftq_idx = new FtqPtr
  val fallThruError = Vec(numDup, Bool())
  val full_pred    = Vec(numDup, new FullBranchPrediction)

  def cfiIndex         = VecInit(full_pred.map(_.cfiIndex))
  def targets(pc: Vec[UInt]) = VecInit(pc.zipWithIndex.map{case (pc, idx) => full_pred(idx).target(pc)})

  def getTarget = targets(pc)

}

/** BPU resp prediction info */
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

/************ BPU <> Ftq ************/
class BpuToFtqIO(implicit p: Parameters) extends WXBundle {
  val resp = DecoupledIO(new BranchPredictionResp())
}

/** Ftq misc information to BPU */
class SpeculativeInfo(implicit p: Parameters) extends WXBundle
  with HasBPUConst with BPUUtils {
  val folded_hist = new AllFoldedHistories(foldedGHistInfos)
  val afhob = new AllAheadFoldedHistoryOldestBits(foldedGHistInfos)
  val lastBrNumOH = UInt((numBr+1).W)
  val histPtr = new CGHPtr
  val ssp = UInt(log2Up(RasSize).W)
  val sctr = UInt(log2Up(RasCtrSize).W)
  val TOSW = new RASPtr
  val TOSR = new RASPtr
  val NOS = new RASPtr
  val topAddr = UInt(VAddrBits.W)
}
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

  val topdown_info    = new FrontendTopDownBundle

  def fromFtqPcBundle(b: Ftq_RF_Components) = {
    this.startAddr := b.startAddr
    this.nextlineStart := b.nextLineAddr
    when(b.fallThruError) {
      // when fallThruError , ifu nextstartAddr is needed to calculate by startAddr or nextLineAddr
      //  the fetch block bytes is the half of cacheblock, so
      //  if startAddr of the fetch block is over the middle of the cacheline,
      //    the nextstartAddr is located at the first half of next cacheline so that the calculated nextstartAddr consist of nextLineAddr & 0,
      //  otherwise,
      //    the nextstartAddr is located at the second half of the same cacheline as startAddr so that the calculated nextstartAddr consist of startAddr & 1,
      val nextBlockHigherTemp = Mux(startAddr(InstFetchBlockOffBits), b.nextLineAddr, b.startAddr)
      val nextBlockHigher = nextBlockHigherTemp(VAddrBits - 1, InstFetchBlockOffBits + 1)
      this.nextStartAddr :=
        Cat(nextBlockHigher,
          startAddr(InstFetchBlockOffBits) ^ 1.U(1.W),
          startAddr(InstFetchBlockOffBits - 1, instOffsetBits),
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
/** IFU predecoder write back info to FTQ */
class PredecodeWritebackBundle(implicit p:Parameters) extends WXBundle {
  // each predict compress instr has its predecode info
  val pc = Vec(PredictWidth, UInt(VAddrBits.W))
  val pd = Vec(PredictWidth, new PreDecodeInfo)
  val ftqIdx = new FtqPtr
  // target: next instr block start addr used for npcGen in the misspred redirect from ifu predecode. which has three cases:
  // 1. Jal instr in fetch block but pred no taken or pred taken after this instr: target is predecode Jal target
  // 2. br / jal in fetch block and pred taken & pred the same jump instr offset But wrong jumpTarget (predTarget != predecode target): target is predecode target (brTarget / jalTarget based on predecode get instr is Br/Jal)
  // 3. excluding the above 2 cases: target is sequential target next misspred instr
  val target = UInt(VAddrBits.W)
  val jalTarget = UInt(VAddrBits.W) // predecode get the jal target from jal instr
  val instrRange = Vec(PredictWidth, Bool()) // indicate whether instr is valid in PredictWidth instr Block
  val misOffset = ValidUndirectioned(UInt(log2Ceil(PredictWidth).W)) // miss prediction determined by predecode
  val cfiOffset = ValidUndirectioned(UInt(log2Ceil(PredictWidth).W)) // cfi offset in instr block. cfi means call frame instr/info, including br/jmp/call/ret
  val ftqOffset = UInt(log2Ceil(PredictWidth).W) // bpu predict cfi Offset sent in ftqToIfu request
}
class IfuToFtqIO(implicit p:Parameters) extends WXBundle {
  val pdWb = Valid(new PredecodeWritebackBundle)
}

/** mmio commit */
class mmioCommitRead(implicit p: Parameters) extends WXBundle {
  val mmioFtqPtr = Output(new FtqPtr)
  val mmioLastCommit = Input(Bool())
}