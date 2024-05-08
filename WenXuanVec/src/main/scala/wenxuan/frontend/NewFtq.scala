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
import wenxuan.common._
import utility._
import utils._
import wenxuan.frontend.icache.HasICacheParameters
import wenxuan.backendInfoType.RedirectLevel
import wenxuan.commonType._

trait HasFtqParameters extends HasICacheParameters with BPUUtils {}

class FtqPtr(implicit p: Parameters) extends CircularQueuePtr[FtqPtr](
  p => p(WXVTileKey).core.ftqSize
){
}

object FtqPtr {
  def apply(f: Bool, v: UInt)(implicit p: Parameters): FtqPtr = {
    val ptr = Wire(new FtqPtr)
    ptr.flag := f
    ptr.value := v
    ptr
  }
  def inverse(ptr: FtqPtr)(implicit p: Parameters): FtqPtr = {
    apply(!ptr.flag, ptr.value)
  }
}

class FtqToICacheIO(implicit p: Parameters) extends WXModule with HasCircularQueuePtrHelper {
  //NOTE: req.bits must be prepare in T cycle
  // while req.valid is set true in T + 1 cycle
  val req = Decoupled(new FtqToICacheRequestBundle)
}

/** use to present prefetch Ptr distance from bpu and ifu respectively */
class PrefetchPtrDB(implicit p: Parameters) extends Bundle {
  val fromBpuPtr  = UInt(log2Up(p(WXVTileKey).core.ftqSize).W)
  val fromIfuPtr  = UInt(log2Up(p(WXVTileKey).core.ftqSize).W)
}

/** use to trace Branch final commit info to present and debug */
class FtqDebugBundle extends Bundle {
  val pc = UInt(39.W)
  val target = UInt(39.W)
  val isBr = Bool()
  val isJmp = Bool()
  val isCall = Bool()
  val isRet = Bool()
  val misPred = Bool()
  val isTaken = Bool()
  val predStage = UInt(2.W)
}

/** use to generate FTB entry of BPU update info */
class FTBEntryGen(implicit p: Parameters) extends WXModule with HasBPUConst {
  val io = IO(new Bundle {
    val start_addr = Input(UInt(VAddrBits.W))
    val old_entry = Input(new FTBEntry)
    val pd = Input(new Ftq_pd_Entry)
    val cfiIndex = Flipped(Valid(UInt(log2Ceil(PredictWidth).W)))
    val target = Input(UInt(VAddrBits.W))
    val hit = Input(Bool())
    val mispredict_vec = Input(Vec(PredictWidth, Bool()))

    val new_entry = Output(new FTBEntry)
    val new_br_insert_pos = Output(Vec(numBr, Bool()))
    val taken_mask = Output(Vec(numBr, Bool()))
    val jmp_taken = Output(Bool())
    val mispred_mask = Output(Vec(numBr+1, Bool()))

    // for perf counters
    val is_init_entry = Output(Bool())
    val is_old_entry = Output(Bool())
    val is_new_br = Output(Bool())
    val is_jalr_target_modified = Output(Bool())
    val is_always_taken_modified = Output(Bool())
    val is_br_full = Output(Bool())
  })

  // no mispredictions detected at predecode
  val hit = io.hit
  val pd = io.pd

  val init_entry = WireInit(0.U.asTypeOf(new FTBEntry))


  val cfi_is_br = pd.brMask(io.cfiIndex.bits) && io.cfiIndex.valid
  val entry_has_jmp = pd.jmpInfo.valid
  val new_jmp_is_jal  = entry_has_jmp && !pd.jmpInfo.bits(0) && io.cfiIndex.valid
  val new_jmp_is_jalr = entry_has_jmp &&  pd.jmpInfo.bits(0) && io.cfiIndex.valid
  val new_jmp_is_call = entry_has_jmp &&  pd.jmpInfo.bits(1) && io.cfiIndex.valid
  val new_jmp_is_ret  = entry_has_jmp &&  pd.jmpInfo.bits(2) && io.cfiIndex.valid
  val last_jmp_rvi = entry_has_jmp && pd.jmpOffset === (PredictWidth-1).U && !pd.rvcMask.last
  // val last_br_rvi = cfi_is_br && io.cfiIndex.bits === (PredictWidth-1).U && !pd.rvcMask.last

  val cfi_is_jal = io.cfiIndex.bits === pd.jmpOffset && new_jmp_is_jal
  val cfi_is_jalr = io.cfiIndex.bits === pd.jmpOffset && new_jmp_is_jalr

  def carryPos = log2Ceil(PredictWidth)+instOffsetBits
  def getLower(pc: UInt) = pc(carryPos-1, instOffsetBits)
  // if not hit, establish a new entry
  init_entry.valid := true.B
  // tag is left for ftb to assign

  // case br
  val init_br_slot = init_entry.getSlotForBr(0)
  when (cfi_is_br) {
    init_br_slot.valid := true.B
    init_br_slot.offset := io.cfiIndex.bits
    init_br_slot.setLowerStatByTarget(io.start_addr, io.target, numBr == 1)
    init_entry.always_taken(0) := true.B // set to always taken on init
  }

  // case jmp
  when (entry_has_jmp) {
    init_entry.tailSlot.offset := pd.jmpOffset
    init_entry.tailSlot.valid := new_jmp_is_jal || new_jmp_is_jalr
    init_entry.tailSlot.setLowerStatByTarget(io.start_addr, Mux(cfi_is_jalr, io.target, pd.jalTarget), isShare=false)
  }

  val jmpPft = getLower(io.start_addr) +& pd.jmpOffset +& Mux(pd.rvcMask(pd.jmpOffset), 1.U, 2.U)
  init_entry.pftAddr := Mux(entry_has_jmp && !last_jmp_rvi, jmpPft, getLower(io.start_addr))
  init_entry.carry   := Mux(entry_has_jmp && !last_jmp_rvi, jmpPft(carryPos-instOffsetBits), true.B)
  init_entry.isJalr := new_jmp_is_jalr
  init_entry.isCall := new_jmp_is_call
  init_entry.isRet  := new_jmp_is_ret
  // that means fall thru points to the middle of an inst
  init_entry.last_may_be_rvi_call := pd.jmpOffset === (PredictWidth-1).U && !pd.rvcMask(pd.jmpOffset)

  // if hit, check whether a new cfi(only br is possible) is detected
  val oe = io.old_entry
  val br_recorded_vec = oe.getBrRecordedVec(io.cfiIndex.bits)
  val br_recorded = br_recorded_vec.asUInt.orR
  val is_new_br = cfi_is_br && !br_recorded
  val new_br_offset = io.cfiIndex.bits
  // vec(i) means new br will be inserted BEFORE old br(i)
  val allBrSlotsVec = oe.allSlotsForBr
  val new_br_insert_onehot = VecInit((0 until numBr).map{
    i => i match {
      case 0 =>
        !allBrSlotsVec(0).valid || new_br_offset < allBrSlotsVec(0).offset
      case idx =>
        allBrSlotsVec(idx-1).valid && new_br_offset > allBrSlotsVec(idx-1).offset &&
          (!allBrSlotsVec(idx).valid || new_br_offset < allBrSlotsVec(idx).offset)
    }
  })

  val old_entry_modified = WireInit(io.old_entry)
  for (i <- 0 until numBr) {
    val slot = old_entry_modified.allSlotsForBr(i)
    when (new_br_insert_onehot(i)) {
      slot.valid := true.B
      slot.offset := new_br_offset
      slot.setLowerStatByTarget(io.start_addr, io.target, i == numBr-1)
      old_entry_modified.always_taken(i) := true.B
    }.elsewhen (new_br_offset > oe.allSlotsForBr(i).offset) {
      old_entry_modified.always_taken(i) := false.B
      // all other fields remain unchanged
    }.otherwise {
      // case i == 0, remain unchanged
      if (i != 0) {
        val noNeedToMoveFromFormerSlot = (i == numBr-1).B && !oe.brSlots.last.valid
        when (!noNeedToMoveFromFormerSlot) {
          slot.fromAnotherSlot(oe.allSlotsForBr(i-1))
          old_entry_modified.always_taken(i) := oe.always_taken(i)
        }
      }
    }
  }

  // two circumstances:
  // 1. oe: | br | j  |, new br should be in front of j, thus addr of j should be new pft
  // 2. oe: | br | br |, new br could be anywhere between, thus new pft is the addr of either
  //        the previous last br or the new br
  val may_have_to_replace = oe.noEmptySlotForNewBr
  val pft_need_to_change = is_new_br && may_have_to_replace
  // it should either be the given last br or the new br
  when (pft_need_to_change) {
    val new_pft_offset =
      Mux(!new_br_insert_onehot.asUInt.orR,
        new_br_offset, oe.allSlotsForBr.last.offset)

    // set jmp to invalid
    old_entry_modified.pftAddr := getLower(io.start_addr) + new_pft_offset
    old_entry_modified.carry := (getLower(io.start_addr) +& new_pft_offset).head(1).asBool
    old_entry_modified.last_may_be_rvi_call := false.B
    old_entry_modified.isCall := false.B
    old_entry_modified.isRet := false.B
    old_entry_modified.isJalr := false.B
  }

  val old_entry_jmp_target_modified = WireInit(oe)
  val old_target = oe.tailSlot.getTarget(io.start_addr) // may be wrong because we store only 20 lowest bits
  val old_tail_is_jmp = !oe.tailSlot.sharing
  val jalr_target_modified = cfi_is_jalr && (old_target =/= io.target) && old_tail_is_jmp // TODO: pass full jalr target
  when (jalr_target_modified) {
    old_entry_jmp_target_modified.setByJmpTarget(io.start_addr, io.target)
    old_entry_jmp_target_modified.always_taken := 0.U.asTypeOf(Vec(numBr, Bool()))
  }

  val old_entry_always_taken = WireInit(oe)
  val always_taken_modified_vec = Wire(Vec(numBr, Bool())) // whether modified or not
  for (i <- 0 until numBr) {
    old_entry_always_taken.always_taken(i) :=
      oe.always_taken(i) && io.cfiIndex.valid && oe.brValids(i) && io.cfiIndex.bits === oe.brOffset(i)
    always_taken_modified_vec(i) := oe.always_taken(i) && !old_entry_always_taken.always_taken(i)
  }
  val always_taken_modified = always_taken_modified_vec.reduce(_||_)



  val derived_from_old_entry =
    Mux(is_new_br, old_entry_modified,
      Mux(jalr_target_modified, old_entry_jmp_target_modified, old_entry_always_taken))


  io.new_entry := Mux(!hit, init_entry, derived_from_old_entry)

  io.new_br_insert_pos := new_br_insert_onehot
  io.taken_mask := VecInit((io.new_entry.brOffset zip io.new_entry.brValids).map{
    case (off, v) => io.cfiIndex.bits === off && io.cfiIndex.valid && v
  })
  io.jmp_taken := io.new_entry.jmpValid && io.new_entry.tailSlot.offset === io.cfiIndex.bits
  for (i <- 0 until numBr) {
    io.mispred_mask(i) := io.new_entry.brValids(i) && io.mispredict_vec(io.new_entry.brOffset(i))
  }
  io.mispred_mask.last := io.new_entry.jmpValid && io.mispredict_vec(pd.jmpOffset)

  // for perf counters
  io.is_init_entry := !hit
  io.is_old_entry := hit && !is_new_br && !jalr_target_modified && !always_taken_modified
  io.is_new_br := hit && is_new_br
  io.is_jalr_target_modified := hit && jalr_target_modified
  io.is_always_taken_modified := hit && always_taken_modified
  io.is_br_full := hit && is_new_br && may_have_to_replace
}

class Ftq(implicit p: Parameters) extends WXModule with HasFtqParameters with HasBPUConst
  with HasCircularQueuePtrHelper with HasPerfEvents // for generatePerfEvent
{
  val io = IO(new Bundle {
    val fromBpu = Flipped(new BpuToFtqIO)
    val fromIfu = Flipped(new IfuToFtqIO)

    val toBpu = new FtqToBpuIO
    val toIfu = new FtqToIfuIO

    val toICache = new FtqToICacheIO
    val toPrefetch = new FtqPrefechBundle

    val fromBackend = Flipped(new CtrlToFtqIO)
    val toBackend = new FtqToCtrlIO


    val bpuInfo = new Bundle {
      val bpRight = Output(UInt(XLEN.W))
      val bpWrong = Output(UInt(XLEN.W))
    }

    val mmioCommitRead = Flipped(new mmioCommitRead)

    // for perf
    val ControlBTBMissBubble = Output(Bool())
    val TAGEMissBubble = Output(Bool())
    val SCMissBubble = Output(Bool())
    val ITTAGEMissBubble = Output(Bool())
    val RASMissBubble = Output(Bool())
  })
  io.bpuInfo := DontCare

  val topdown_stage = RegInit(0.U.asTypeOf(new FrontendTopDownBundle))
  // only driven by clock, not valid-ready
  topdown_stage := io.fromBpu.resp.bits.topdown_info
  io.toIfu.req.bits.topdown_info := topdown_stage

  /** *************************************************************************
   *  **************************** Redirect Bundle ****************************
   *  ************************************************************************* */
  // io.fromBackend.ftqIdxAhead: jmp + alu(aluCnt) + ldReplay + exception
  val aluAheadStart = 1
  // only select redirect ahead from alu
  val ftqIdxAhead = VecInit(Seq.tabulate(FtqRedirectAheadNum)(i => io.fromBackend.ftqIdxAhead(i + aluAheadStart)))
  val ftqIdxSelOH = io.fromBackend.ftqIdxSelOH.bits(FtqRedirectAheadNum, 1)
  val aheadValid = ftqIdxAhead.map(_.valid).reduce(_ | _) && !io.fromBackend.redirect.valid
  val realAhdValid = io.fromBackend.redirect.valid && (ftqIdxSelOH > 0.U) && RegNext(aheadValid)

  /** ******************** Redirect from Backend & IFU ********************
   *  Both Redirect type are need two stage to complete So both need to register */
  val backendRedirect = Wire(Valid(new BranchPredictionRedirect))
  val backendRedirectReg = RegNext(backendRedirect)
  backendRedirectReg.valid := Mux(realAhdValid, 0.B, backendRedirect.valid)
  val fromBackendRedirect = Wire(Valid(new BranchPredictionRedirect))
  fromBackendRedirect := Mux(realAhdValid, backendRedirect, backendRedirectReg)

  val stage2Flush = backendRedirect.valid
  val backendFlush = stage2Flush || RegNext(stage2Flush)
  val flush = stage2Flush || RegNext(stage2Flush)

  val ifuRedirected = RegInit(VecInit(Seq.fill(FtqSize)(false.B)))

  // stage2 of ifu redirect act on BPU and storage about BPU in ftq, like cfiIndex_vec, b_p2_newest...
  val fromIfuRedirect = WireInit(0.U.asTypeOf(Valid(new BranchPredictionRedirect)))
  val ifuRedirectReg = RegNext(fromIfuRedirect, init = 0.U.asTypeOf(Valid(new BranchPredictionRedirect)))
  val ifuRedirectToBpu = WireInit(ifuRedirectReg) // stage2 act on bpu and storage
  val ifuFlush = Wire(Bool())
  ifuFlush := fromIfuRedirect.valid || ifuRedirectToBpu.valid

  /** Both Backend redirect and ifu redirect are two cycle
   *   So allowBpuIn/allowToIfu need to combine both two stages */
  val allowToIfu = WireInit(false.B)
  val flushToIfu = !allowToIfu
  allowToIfu := !ifuFlush && !backendRedirect.valid && !backendRedirectReg.valid
  val allowBpuIn = !ifuFlush && !backendRedirect.valid && !backendRedirectReg.valid

  /** **********************************************************************************
  // *************************** redirect ptr and state queue **************************
  // *********************************************************************************** */
  val redirectVec = VecInit(backendRedirect, fromIfuRedirect)
  // backendRedirect has higher priority than fromIfuRedirect in redirectVec
  val redirect_sel = PriorityMux(redirectVec.map(r => (r.valid -> r.bits)))
  val redirect_excl_Ifu_valid = redirectVec.dropRight(1).map(r => r.valid).reduce(_ || _) // valid exclude IFU redirect
  val r_sel_idx = redirect_sel.ftqIdx
  val r_next_idx = r_sel_idx + 1.U

  // flush commitStateQueue at redirect stage2
  when(RegNext(redirectVec.map(r => r.valid).reduce(_ || _))) {
    val (redirect_offset, flushItSelf) = (redirect_sel.ftqOffset, RedirectLevel.flushItself(redirect_sel.level))
    when(RegNext(redirect_excl_Ifu_valid)) {
      // flush(invalid) the instr after the redirect instr and flush itself if "flushItSelf" is true
      commitStateQueue(RegNext(r_sel_idx.value)).zipWithIndex.foreach({ case (s, i) =>
        when(i.U > RegNext(redirect_offset) || i.U === RegNext(redirect_offset) && RegNext(flushItSelf)) {
          s := c_invalid
        }
      })
    }
  }

  def copyNum = 5 // copyNum for ICache

  // **************************** Commit Ptr ******************************
  val commPtr = RegInit(FtqPtr(false.B, 0.U))
  val commPtrPlus1 = RegInit(FtqPtr(false.B, 1.U))
  val commPtr_next = WireInit(commPtr)
  val commPtrPlus1_next = WireInit(commPtrPlus1)
  commPtr := commPtr_next
  commPtrPlus1 := commPtrPlus1_next

  val canCommit = Wire(Bool())
  // **********************************************************************
  // **************************** BPU to FTQ ******************************
  // **********************************************************************
  val bpuPtr = RegInit(FtqPtr(false.B, 0.U))
  val copied_bpu_ptr = Seq.fill(copyNum)(RegInit(FtqPtr(false.B, 0.U)))
  io.toBpu.enq_ptr := bpuPtr
  // from BPU
  val validEntries = distanceBetween(bpuPtr, commPtr)
  val new_entry_ready = validEntries < FtqSize.U || canCommit // enq from bpu
  io.fromBpu.resp.ready := new_entry_ready

  val bpu_s2_resp = io.fromBpu.resp.bits.s2
  val bpu_s3_resp = io.fromBpu.resp.bits.s3
  val bpu_s2_redirect = bpu_s2_resp.valid(3) && bpu_s2_resp.hasRedirect(3)
  val bpu_s3_redirect = bpu_s3_resp.valid(3) && bpu_s3_resp.hasRedirect(3)

  val b_p0_bpu_enq_fire = io.fromBpu.resp.fire && allowBpuIn // from bpu s1/s2/s3
  val b_p0_bpu_in_fire = (io.fromBpu.resp.fire || bpu_s2_redirect || bpu_s3_redirect) && allowBpuIn
  val b_p0_bpu_in_resp = io.fromBpu.resp.bits.selectedResp // Mux
  val b_p0_bpu_in_stage = io.fromBpu.resp.bits.selectedRespIdxForFtq // Mux
  val b_p0_bpu_in_resp_ptr = Mux(b_p0_bpu_in_stage === BP_S1, bpuPtr, b_p0_bpu_in_resp.ftq_idx)
  val b_p0_bpu_in_resp_idx = b_p0_bpu_in_resp_ptr.value

  // **********************************************************************
  // **************************** BPU Ptr Update **************************
  // **********************************************************************
  // when redirect, we should reset ptrs and status queues
  when(redirectVec.map(r => r.valid).reduce(_ || _)) { // redirect is highest priority
    bpuPtr := r_next_idx
    copied_bpu_ptr.map(_ := r_next_idx)
  }.elsewhen(bpu_s3_redirect) { // BPU s3 redirect is the next highest priority
    bpuPtr := bpu_s3_resp.ftq_idx + 1.U
    copied_bpu_ptr.map(_ := bpu_s3_resp.ftq_idx + 1.U)
  }.elsewhen(bpu_s2_redirect) { // BPU s2 redirect
    bpuPtr := bpu_s2_resp.ftq_idx + 1.U
    copied_bpu_ptr.map(_ := bpu_s2_resp.ftq_idx + 1.U)
  }.otherwise { // normal increment when bpu prediction info enqueue except s2/s3 redirect
    bpuPtr := bpuPtr + b_p0_bpu_enq_fire
    copied_bpu_ptr.map(_ := bpuPtr + b_p0_bpu_enq_fire)
  }

  // Reg storage, sync read ports: prefetchReq ++  ifuReq1 + ifuReq2 + ifuReq3 + commitUpdate2 + commitUpdate
  val ftq_pc_mem = Module(new FtqPcMemWrapper(1)) // 1 otherReads port for prefetch
  // bpu prediction address is writing to ftq_pc_mem
  ftq_pc_mem.io.wen := b_p0_bpu_in_fire
  ftq_pc_mem.io.waddr := b_p0_bpu_in_resp_idx
  ftq_pc_mem.io.wdata.fromBranchPrediction(b_p0_bpu_in_resp)

  /** store the information about the speculative updated BPU structure like RAS and hist */
  //                                                                           ifuRedirect + backendRedirect + commit
  val ftq_redirect_sram = Module(new FtqNRSRAM(new Ftq_Redirect_SRAMEntry, numRead = 1 + FtqRedirectAheadNum + 1))
  // these info is intended to enq at the last stage of bpu
  ftq_redirect_sram.io.wen := io.fromBpu.resp.bits.lastStage.valid(3)
  ftq_redirect_sram.io.waddr := io.fromBpu.resp.bits.lastStage.ftq_idx.value
  ftq_redirect_sram.io.wdata := io.fromBpu.resp.bits.last_stage_spec_info
  println(f"ftq redirect SRAM: entry ${ftq_redirect_sram.io.wdata.getWidth} * ${FtqSize} * 3")
  println(f"ftq redirect SRAM: ahead fh ${ftq_redirect_sram.io.wdata.afhob.getWidth} * ${FtqSize} * 3")

  /** store *last_stage_meta* concatenated from all meta info output by each BPU, like TAGE tag, etc. */
  val ftq_meta_1r_sram = Module(new FtqNRSRAM(new Ftq_1R_SRAMEntry, numRead = 1))
  // these info is intended to enq at the last stage of bpu
  ftq_meta_1r_sram.io.wen := io.fromBpu.resp.bits.lastStage.valid(3)
  ftq_meta_1r_sram.io.waddr := io.fromBpu.resp.bits.lastStage.ftq_idx.value
  ftq_meta_1r_sram.io.wdata.meta := io.fromBpu.resp.bits.last_stage_meta

  /** FTB entry info storage used to train FTB */
  // Reg storage, sync read ports:                                                  ifuRedirect + backendRedirect + commit
  val ftb_entry_mem = Module(new SyncDataModuleTemplate(new FTBEntry, FtqSize, numRead = 1 + FtqRedirectAheadNum + 1, 1))
  ftb_entry_mem.io.wen(0) := io.fromBpu.resp.bits.lastStage.valid(3)
  ftb_entry_mem.io.waddr(0) := io.fromBpu.resp.bits.lastStage.ftq_idx.value
  ftb_entry_mem.io.wdata(0) := io.fromBpu.resp.bits.last_stage_ftb_entry

  // Reg Array , multi-write
  val update_target = Reg(Vec(FtqSize, UInt(VAddrBits.W))) // could be taken target or fallThrough //TODO: remove this
  val b_p2_bpu_newest_entry_target = Reg(UInt(VAddrBits.W))
  val b_p2_bpu_newest_entry_ptr = Reg(new FtqPtr)
  val cfiIndex_vec = Reg(Vec(FtqSize, ValidUndirectioned(UInt(log2Ceil(PredictWidth).W))))
  val mispredict_vec = Reg(Vec(FtqSize, Vec(PredictWidth, Bool())))
  val pred_stage = Reg(Vec(FtqSize, UInt(2.W)))
  val pred_s1_cycle = if (!env.FPGAPlatform) Some(Reg(Vec(FtqSize, UInt(64.W)))) else None

  val c_invalid :: c_valid :: c_commited :: Nil = Enum(3)
  val commitStateQueue = RegInit(VecInit(Seq.fill(FtqSize) {
    VecInit(Seq.fill(PredictWidth)(c_invalid))
  }))

  // send status related to ifu & bpu
  val f_to_send :: f_sent :: Nil = Enum(2)
  val entry_fetch_status = RegInit(VecInit(Seq.fill(FtqSize)(f_sent)))

  // record FTB entry hit status, false_hit means the hit but pred error situation
  val h_not_hit :: h_false_hit :: h_hit :: Nil = Enum(3)
  val entry_hit_status = RegInit(VecInit(Seq.fill(FtqSize)(h_not_hit)))
  // only use ftb result to assign hit status
  when(bpu_s2_resp.valid(3)) {
    entry_hit_status(bpu_s2_resp.ftq_idx.value) := Mux(bpu_s2_resp.full_pred(3).hit, h_hit, h_not_hit)
  }

  // modify registers one cycle later to cut critical path
  val b_p1_bpu_resp_fire = RegNext(b_p0_bpu_in_fire)
  val b_p1_bpu_resp_ptr = RegNext(b_p0_bpu_in_resp_ptr)
  val b_p1_bpu_resp_idx = b_p1_bpu_resp_ptr.value
  val b_p1_bpu_resp_target = RegNext(b_p0_bpu_in_resp.getTarget(3))
  val b_p1_bpu_resp_cfiIndex = RegNext(b_p0_bpu_in_resp.cfiIndex(3))
  val b_p1_bpu_resp_stage = RegNext(b_p0_bpu_in_stage)

  // p1 update, p2 take effect
  when(b_p1_bpu_resp_fire) {
    entry_fetch_status(b_p1_bpu_resp_idx) := f_to_send
    cfiIndex_vec(b_p1_bpu_resp_idx) := b_p1_bpu_resp_cfiIndex
    pred_stage(b_p1_bpu_resp_idx) := b_p1_bpu_resp_stage
    update_target(b_p1_bpu_resp_idx) := b_p1_bpu_resp_target // TODO: remove this
    b_p2_bpu_newest_entry_target := b_p1_bpu_resp_target
    b_p2_bpu_newest_entry_ptr := b_p1_bpu_resp_ptr
  }

  // reduce fanout by delay write for a cycle
  when(RegNext(b_p1_bpu_resp_fire)) {
    mispredict_vec(RegNext(b_p1_bpu_resp_idx)) := WireInit(VecInit(Seq.fill(PredictWidth)(false.B)))
  }

  // record s1 pred cycles
  pred_s1_cycle.map(vec => {
    when(b_p0_bpu_in_fire && (b_p0_bpu_in_stage === BP_S1)) {
      vec(b_p0_bpu_in_resp_idx) := b_p0_bpu_in_resp.full_pred(0).predCycle.getOrElse(0.U)
    }
  })

  /** ****************************************************************
  // ************** BPU enq Update commit State Queue ****************
  // **************************************************************** */

  /** reduce fanout using copied b_p1_bpu_resp_fire and copied b_p1_bpu_resp_ptr
   *   In order to reduce fanout, the 64-item commit State Queue is divided into two Sets,
   *   and two compare circuits are placed respectively to judge the update of the commit State Queue
   */
  def copyNum_for_commitStateQueue = 2 // copy 2 bpu_in and bpu_in_ptr information used for 2 sets queue compare&update circuits
  val copied_last_cycle_bpu_in_for_ftq = VecInit(Seq.fill(copyNum_for_commitStateQueue)(RegNext(b_p0_bpu_in_fire)))
  val copied_last_cycle_bpu_in_ptr_for_ftq = VecInit(Seq.fill(copyNum_for_commitStateQueue)(RegNext(b_p0_bpu_in_resp_ptr)))

  copied_last_cycle_bpu_in_for_ftq.zip(copied_last_cycle_bpu_in_ptr_for_ftq).zipWithIndex.map {
    case ((in, ptr), i) =>
      when(in) {
        val perSetEntries = FtqSize / copyNum_for_commitStateQueue // 32
        require(FtqSize % copyNum_for_commitStateQueue == 0)
        // compare & update circuits of each commit state queue set
        for (j <- 0 until perSetEntries) {
          when(ptr.value === (i * perSetEntries + j).U) {
            commitStateQueue(i * perSetEntries + j) := VecInit(Seq.fill(PredictWidth)(c_invalid))
          }
        }
      }
  }








  // ****************************************************************
  // ************************* FTQ to IFU ***************************
  // ****************************************************************
  val ifuPtr = RegInit(FtqPtr(false.B, 0.U))
  val ifuPtrPlus1 = RegInit(FtqPtr(false.B, 1.U))
  val ifuPtrPlus2 = RegInit(FtqPtr(false.B, 2.U))
  val copied_ifu_ptr = Seq.fill(copyNum)(RegInit(FtqPtr(false.B, 0.U)))
  require(FtqSize >= 4)
  val ifuPtr_next = WireInit(ifuPtr)
  val ifuPtrPlus1_next = WireInit(ifuPtrPlus1)
  val ifuPtrPlus2_next = WireInit(ifuPtrPlus2)
  ifuPtr := ifuPtr_next
  ifuPtrPlus1 := ifuPtrPlus1_next
  ifuPtrPlus2 := ifuPtrPlus2_next
  copied_ifu_ptr.map { ptr =>
    ptr := ifuPtr_next
    dontTouch(ptr)
  }

  // **********************************************************************
  // ************************** IFU Ptr Update ****************************
  // **********************************************************************
  // when redirect, we should reset ptrs and status queues
  when(redirectVec.map(r => r.valid).reduce(_ || _)) {
    ifuPtr_next := r_next_idx
    ifuPtrPlus1_next := r_next_idx + 1.U
    ifuPtrPlus2_next := r_next_idx + 2.U
  }.elsewhen(bpu_s3_redirect) {
    // only when ifuPtr runs ahead of bpu s2 resp should we recover it
    when(!isBefore(ifuPtr, bpu_s3_resp.ftq_idx)) {
      ifuPtr_next := bpu_s3_resp.ftq_idx
      ifuPtrPlus1_next := bpu_s3_resp.ftq_idx + 1.U
      ifuPtrPlus2_next := bpu_s3_resp.ftq_idx + 2.U
    }
  }.elsewhen(bpu_s2_redirect) {
    // only when ifuPtr runs ahead of bpu s2 resp should we recover it
    when(!isBefore(ifuPtr, bpu_s2_resp.ftq_idx)) {
      ifuPtr_next := bpu_s2_resp.ftq_idx
      ifuPtrPlus1_next := bpu_s2_resp.ftq_idx + 1.U
      ifuPtrPlus2_next := bpu_s2_resp.ftq_idx + 2.U
    }
  }.elsewhen(io.toIfu.req.fire && allowToIfu) { // normal increment when toIfu sent
    ifuPtr_next := ifuPtrPlus1
    ifuPtrPlus1_next := ifuPtrPlus2
    ifuPtrPlus2_next := ifuPtrPlus2 + 1.U
  }

  XSError(isBefore(bpuPtr, ifuPtr) && !isFull(bpuPtr, ifuPtr), "\nifuPtr is before bpuPtr!\n")
  (0 until copyNum).map { i =>
    XSError(copied_bpu_ptr(i) =/= bpuPtr, "\ncopiedBpuPtr is different from bpuPtr!\n")
  }
  // read pc and target: use _next to read ftq_pc_mem
  //  which return rdata equivalent to the updated Ptr corresponding location data
  // _next --> rdata
  //      |
  //       --> Ptr
  ftq_pc_mem.io.ifuPtr_n := ifuPtr_next
  ftq_pc_mem.io.ifuPtrPlus1_n := ifuPtrPlus1_next
  ftq_pc_mem.io.ifuPtrPlus2_n := ifuPtrPlus2_next
  // Pipe 1 return rdata, ifu pipe stage p1, use 'i_p1/2' to distinguish to bpu pipe 'b_p0/1/2'
  // p0               p1                             p2
  // ifuPtr_next      ifuPtr(updated)                RegNext(rdata)
  // read ftq_pc      Ptr_next read return rdata
  //                  Ptr read status return
  val i_p1_pc_mem_ifuPtr_rdata = ftq_pc_mem.io.ifuPtr_rdata
  val i_p1_pc_mem_ifuPtrPlus1_rdata = ftq_pc_mem.io.ifuPtrPlus1_rdata
  val i_p1_pc_mem_ifuPtrPlus2_rdata = ftq_pc_mem.io.ifuPtrPlus2_rdata

  // BPU bypass duplicate : 0~copyNum-1 for ICache, last one for ifu
  // s1 : bpu pipe stage 1
  val b_p1_bpu_wr_bypass_dup = VecInit(Seq.fill(copyNum+1)(RegEnable(ftq_pc_mem.io.wdata, b_p0_bpu_in_fire)))
  val b_p1_bpu_wr_bypass_for_ifu = b_p1_bpu_wr_bypass_dup.last
  val b_p1_bpu_resp_ptr_bypass_dup = VecInit(Seq.fill(copyNum+1)(RegNext(b_p0_bpu_in_resp_ptr)))
  val b_p1_bpu_resp_ptr_bypass_for_ifu = b_p1_bpu_resp_ptr_bypass_dup.last

  // toIfu fire so that ifuPtr(Plusx) increase in next cycle if **no redirect**
  // p0: ifuPtrPlus_next read
  // i_p1: ifuPtrPlus1 corresponds to i_p1 ifuPtrPlus1 rdata, if toIfu fire & no redirect,
  //   ifuPtr_next(:= ifuPtrPlus1) corr to i_p1 ifuPtrPlus1 rdata. (corr. means corresponds)
  // i_p2: so if i_p1 toIfu fire, ifuPtr corr to ifuPtrPlus1 rdata, as the same, ifuPtrPlus1 corr to ifuPtrPlus2 rdata.
  // rdata & stat info duplicate : 0~copyNum-1 for ICache, last one for ifu
  val i_p2_pc_mem_data_corr_ifuPtr_dup = VecInit(Seq.fill(copyNum+1)(RegNext(
    Mux(io.toIfu.req.fire, // only used in no redirect condition(.otherwise statement) so no need to add condition in here
      i_p1_pc_mem_ifuPtrPlus1_rdata,
      i_p1_pc_mem_ifuPtr_rdata)// no toIfu fire, p1 ifuPtr rdata still corr to ifuPtr in p2
  )))
  val i_p2_pc_mem_data_corr_ifuPtr_ifu = i_p2_pc_mem_data_corr_ifuPtr_dup.last

  val i_p2_pc_mem_data_corr_ifuPtrPlus1_dup = VecInit(Seq.fill(copyNum+1)(RegNext(
    Mux(io.toIfu.req.fire,
      i_p1_pc_mem_ifuPtrPlus2_rdata,
      i_p1_pc_mem_ifuPtrPlus1_rdata)// no toIfu fie, p1 ifuPtrPlus1 rdata still corr to ifuPtrPlus1 in p2
  )))
  val i_p2_pc_mem_data_corr_ifuPtrPlus1_ifu = i_p2_pc_mem_data_corr_ifuPtrPlus1_dup.last

  val i_p1_ifuPtrPlus1_to_send = (entry_fetch_status(ifuPtrPlus1.value) === f_to_send) ||
                   (b_p1_bpu_resp_fire && b_p1_bpu_resp_ptr_bypass_for_ifu === ifuPtrPlus1)
  val i_p1_ifuPtr_to_send = (entry_fetch_status(ifuPtr.value) === f_to_send) ||
                   (b_p1_bpu_resp_fire && b_p1_bpu_resp_ptr_bypass_for_ifu === ifuPtr)
  val i_p2_to_send_stat_corr_ifuPtr_dup = VecInit(Seq.fill(copyNum+1)(RegNext(
    Mux(io.toIfu.req.fire,
      i_p1_ifuPtrPlus1_to_send,
      i_p1_ifuPtr_to_send)
  )))
  val i_p2_to_send_stat_corr_ifuPtr_ifu = i_p2_to_send_stat_corr_ifuPtr_dup.last

  val toIfuPcBundle = Wire(new Ftq_RF_Components)
  val entry_is_to_send = WireInit(entry_fetch_status(ifuPtr.value) === f_to_send)
  val entry_ftq_offset = WireInit(cfiIndex_vec(ifuPtr.value))
  val entry_next_addr = Wire(UInt(VAddrBits.W))
  val diff_entry_next_addr = WireInit(update_target(ifuPtr.value)) //TODO: remove this

  // TODO: reconsider target address bypass logic
  when(b_p1_bpu_resp_fire && b_p1_bpu_resp_ptr_bypass_for_ifu === ifuPtr) { // ifuPtr hit BPU s1 stage write info
    // at the beginning or redirect, ifuPtr will go to this situation
    toIfuPcBundle := b_p1_bpu_wr_bypass_for_ifu
    entry_is_to_send := true.B
    entry_next_addr := b_p1_bpu_resp_target
    entry_ftq_offset := b_p1_bpu_resp_cfiIndex
    diff_entry_next_addr := b_p1_bpu_resp_target // TODO: remove this
  }.otherwise { // exclude redirect
    toIfuPcBundle := i_p2_pc_mem_data_corr_ifuPtr_ifu
    entry_is_to_send := i_p2_to_send_stat_corr_ifuPtr_ifu // reduce potential bubbles
    entry_next_addr := Mux(b_p1_bpu_resp_fire && b_p1_bpu_resp_ptr_bypass_for_ifu === ifuPtrPlus1, // p2 ifuPtrPlus1 (=== ifuPtr + 1) hit BPU s1
                          b_p1_bpu_wr_bypass_for_ifu.startAddr,
                          Mux(ifuPtr === b_p2_bpu_newest_entry_ptr, // p2 ifuPtr hit BPU s2 stage
                            b_p2_bpu_newest_entry_target,
                            i_p2_pc_mem_data_corr_ifuPtrPlus1_ifu.startAddr))
  }

  io.toIfu.req.valid := entry_is_to_send && ifuPtr =/= bpuPtr
  io.toIfu.req.bits.fromFtqPcBundle(toIfuPcBundle)
  io.toIfu.req.bits.ftqIdx := ifuPtr
  io.toIfu.req.bits.nextStartAddr := entry_next_addr
  io.toIfu.req.bits.ftqOffset := entry_ftq_offset

  // flush by BPU redirect
  io.toIfu.flushFromBpu.s2.valid := bpu_s2_redirect
  io.toIfu.flushFromBpu.s2.bits := bpu_s2_resp.ftq_idx
  io.toIfu.flushFromBpu.s3.valid := bpu_s3_redirect
  io.toIfu.flushFromBpu.s3.bits := bpu_s3_resp.ftq_idx

  val ifu_req_should_be_flushed =
    io.toIfu.flushFromBpu.shouldFlushByStage2(io.toIfu.req.bits.ftqIdx) ||
      io.toIfu.flushFromBpu.shouldFlushByStage3(io.toIfu.req.bits.ftqIdx)

  when(io.toIfu.req.fire && !ifu_req_should_be_flushed) {
    entry_fetch_status(ifuPtr.value) := f_sent
  }
  // TODO: remove this
  XSError(io.toIfu.req.valid && diff_entry_next_addr =/= entry_next_addr,
    p"\nifu_req_target wrong! ifuPtr: ${ifuPtr}, entry_next_addr: ${Hexadecimal(entry_next_addr)} diff_entry_next_addr: ${Hexadecimal(diff_entry_next_addr)}\n")

  // when fall through is smaller in value than start address, there must be a false hit
  when(toIfuPcBundle.fallThruError && entry_hit_status(ifuPtr.value) === h_hit) {
    when(io.toIfu.req.fire &&
      !(bpu_s2_redirect && bpu_s2_resp.ftq_idx === ifuPtr) && // why no ftq_idx <= ifuPtr ?
      !(bpu_s3_redirect && bpu_s3_resp.ftq_idx === ifuPtr)
    ) {
      entry_hit_status(ifuPtr.value) := h_false_hit
      // XSError(true.B, "FTB false hit by fallThroughError, startAddr: %x, fallTHru: %x\n", io.toIfu.req.bits.startAddr, io.toIfu.req.bits.nextStartAddr)
    }
    XSDebug(true.B, "fallThruError! start:%x, fallThru:%x\n", io.toIfu.req.bits.startAddr, io.toIfu.req.bits.nextStartAddr)
  }

  XSPerfAccumulate(f"fall_through_error_to_ifu", toIfuPcBundle.fallThruError && entry_hit_status(ifuPtr.value) === h_hit &&
    io.toIfu.req.fire && !(bpu_s2_redirect && bpu_s2_resp.ftq_idx === ifuPtr) && !(bpu_s3_redirect && bpu_s3_resp.ftq_idx === ifuPtr))

  // ****************************************************************
  // ************************* FTQ <> ICache ************************
  // ****************************************************************
  val b_p1_bpu_resp_fire_dup = VecInit(Seq.fill(copyNum)(RegNext(b_p0_bpu_in_fire)))

  val toICachePcBundle = Wire(Vec(copyNum, new Ftq_RF_Components))
  val toICacheEntryToSend = Wire(Vec(copyNum, Bool()))

  for (i <- 0 until copyNum) { // 0~copyNum for ICache
    when(b_p1_bpu_resp_fire_dup(i) && b_p1_bpu_resp_ptr_bypass_dup(i) === copied_ifu_ptr(i)) {
      toICachePcBundle(i) := b_p1_bpu_wr_bypass_dup(i)
      toICacheEntryToSend(i) := true.B
    }.otherwise {
      toICachePcBundle(i) := i_p2_pc_mem_data_corr_ifuPtr_dup(i)
      toICacheEntryToSend(i) := i_p2_to_send_stat_corr_ifuPtr_dup(i)
    }
  }
  io.toICache.req.valid := entry_is_to_send && ifuPtr =/= bpuPtr
  io.toICache.req.bits.readValid.zipWithIndex.foreach { case (copy, i) =>
    copy := toICacheEntryToSend(i) && copied_ifu_ptr(i) =/= copied_bpu_ptr(i) }
  io.toICache.req.bits.pcMemRead.zipWithIndex.map { case (copy, i) => copy.fromFtqPcBundle(toICachePcBundle(i)) }

  // *********************************************************************
  // **************************** wb from ifu ****************************
  // *********************************************************************
  val ifuWbPtr = RegInit(FtqPtr(false.B, 0.U))
  val ifuWbPtr_next = WireInit(ifuWbPtr)
  ifuWbPtr := ifuWbPtr_next

  val pdWb = io.fromIfu.pdWb
  val iwb_p0_pdWb = pdWb.bits
  val iwb_p0_pdWb_valid = pdWb.valid
  val iwb_p0_pdWb_instr_info = pdWb.bits.pd // every instr decode info in the PredictWidth instr block
  val iwb_p0_pdWb_corr_ftqIdx = pdWb.bits.ftqIdx.value
  /** Store predecode jump/branch information from IFU and Used by Update BPU */
  // Reg Storage, read ports:                                                 commit update
  val ftq_pd_mem = Module(new SyncDataModuleTemplate(new Ftq_pd_Entry, FtqSize, 1, 1))
  ftq_pd_mem.io.wen(0) := iwb_p0_pdWb_valid
  ftq_pd_mem.io.waddr(0) := iwb_p0_pdWb_corr_ftqIdx
  ftq_pd_mem.io.wdata(0).fromPdWb(iwb_p0_pdWb)

  // ************************ IFU WB Ptr Update ***************************
  when(redirectVec.map(r => r.valid).reduce(_ || _)) {
    ifuWbPtr_next := r_next_idx
  }.elsewhen(iwb_p0_pdWb_valid) {
    ifuWbPtr_next := ifuWbPtr + 1.U
  }
  XSError(isBefore(ifuWbPtr, commPtr) && !isFull(ifuWbPtr, commPtr), "\ncommPtr is before ifuWbPtr!\n")

  // ********************* commitStateQueue Updated by ifu wb ************************
  // Use predecode info to indicate valid instr which needed to consider in commit stage
  when(iwb_p0_pdWb_valid) {
    val comm_stq_wen = VecInit(iwb_p0_pdWb_instr_info.map(_.valid).zip(iwb_p0_pdWb.instrRange).map {
      case (v, inRange) => v && inRange
    })
    (commitStateQueue(iwb_p0_pdWb_corr_ftqIdx) zip comm_stq_wen).map {
      case (qe, v) => when(v) {
        qe := c_valid
      }
    }
  }

  XSError(iwb_p0_pdWb_valid && isAfter(pdWb.bits.ftqIdx, ifuPtr), "IFU returned a predecode before its req, check IFU")

  // predecode writeback info corresponding to the FTB entry hit status of the instr block
  val iwb_p0_pdWb_corr_hit_stat = entry_hit_status(iwb_p0_pdWb_corr_ftqIdx) === h_hit && iwb_p0_pdWb_valid
  // FTB entry hit and determined miss predict by predecode
  val iwb_p0_pdWb_corr_hitFtb_mispred = iwb_p0_pdWb_corr_hit_stat && iwb_p0_pdWb.misOffset.valid

  // ifu wb pipe1
  val iwb_p1_pdWb_corr_hit_stat = RegNext(iwb_p0_pdWb_corr_hit_stat)
  val iwb_p1_pdWb_corr_hitFtb_mispred = RegNext(iwb_p0_pdWb_corr_hitFtb_mispred, init = false.B)
  val iwb_p1_pdWb_instrs_info = RegEnable(iwb_p0_pdWb_instr_info, iwb_p0_pdWb_valid)
  val iwb_p1_pdWb_corr_ftqIdx = RegEnable(iwb_p0_pdWb_corr_ftqIdx, iwb_p0_pdWb_valid)

  ftb_entry_mem.io.raddr.head := iwb_p0_pdWb_corr_ftqIdx // pipe0 read, pipe1 return rdata
  val has_false_hit = WireInit(false.B)
  when(iwb_p1_pdWb_corr_hit_stat) {
    // compare hit FTB entry slot predict info with the predecode info
    // indexed by offset provided by hit FTB entry slot , check for false hit
    val pred_ftb_entry = ftb_entry_mem.io.rdata.head
    val brSlots = pred_ftb_entry.brSlots
    val tailSlot = pred_ftb_entry.tailSlot
    val tailSlot_valid_for_br = tailSlot.valid && tailSlot.sharing
    val tailSlot_valid_for_jmp = tailSlot.valid && !tailSlot.sharing
    // we check cfis that bpu predicted
    // bpu predicted branches but denied by predecode
    val br_false_hit =
      brSlots.map {
        s => s.valid && !(iwb_p1_pdWb_instrs_info(s.offset).valid && iwb_p1_pdWb_instrs_info(s.offset).isBr)
      }.reduce(_ || _) ||
        (tailSlot_valid_for_br &&
          !(iwb_p1_pdWb_instrs_info(tailSlot.offset).valid && iwb_p1_pdWb_instrs_info(tailSlot.offset).isBr))

    val jmpOffset = tailSlot.offset
    val jmp_pd = iwb_p1_pdWb_instrs_info(jmpOffset)
    val jal_false_hit = tailSlot_valid_for_jmp &&
      ((pred_ftb_entry.isJal && !(jmp_pd.valid && jmp_pd.isJal)) ||
        (pred_ftb_entry.isJalr && !(jmp_pd.valid && jmp_pd.isJalr)) ||
        (pred_ftb_entry.isCall && !(jmp_pd.valid && jmp_pd.isCall)) ||
        (pred_ftb_entry.isRet && !(jmp_pd.valid && jmp_pd.isRet))
      )

    has_false_hit := br_false_hit || jal_false_hit || iwb_p1_pdWb_corr_hitFtb_mispred
    XSDebug(has_false_hit, "FTB false hit by br or jal or hit_pd, startAddr: %x\n", pdWb.bits.pc(0))

    // assert(!has_false_hit)
  }

  // ifu wb pipe1 update, pipe2 takes effect
  when(has_false_hit) {
    entry_hit_status(iwb_p1_pdWb_corr_ftqIdx) := h_false_hit
  }

  // **********************************************************************
  // ***************************** to backend *****************************
  // **********************************************************************
  // to backend newest bpu enqueue info, num cycle is fixed
  io.toBackend.newest_entry_ptr := RegNext(b_p2_bpu_newest_entry_ptr)
  io.toBackend.newest_entry_target := RegNext(b_p2_bpu_newest_entry_target)
  // to backend pc mem / target
  io.toBackend.pc_mem_wen := RegNext(b_p1_bpu_resp_fire)
  io.toBackend.pc_mem_waddr := RegNext(b_p1_bpu_resp_idx)
  io.toBackend.pc_mem_wdata := RegNext(b_p1_bpu_wr_bypass_for_ifu)

  // *******************************************************************************
  // **************************** redirect from backend ****************************
  // *******************************************************************************

  // redirect read cfiInfo, couples to redirectGen s2
  val redirectReadStart = 1 // 0 for ifuRedirect
  val ftq_redirect_rdata = Wire(Vec(FtqRedirectAheadNum, new Ftq_Redirect_SRAMEntry))
  val ftb_redirect_rdata = Wire(Vec(FtqRedirectAheadNum, new FTBEntry))
  for (i <- redirectReadStart until FtqRedirectAheadNum) {
    ftq_redirect_sram.io.ren(i + redirectReadStart) := ftqIdxAhead(i).valid
    ftq_redirect_sram.io.raddr(i + redirectReadStart) := ftqIdxAhead(i).bits.value
    ftb_entry_mem.io.raddr(i + redirectReadStart) := ftqIdxAhead(i).bits.value
  }
  ftq_redirect_sram.io.ren(redirectReadStart) := Mux(aheadValid, ftqIdxAhead(0).valid, backendRedirect.valid)
  ftq_redirect_sram.io.raddr(redirectReadStart) := Mux(aheadValid, ftqIdxAhead(0).bits.value, backendRedirect.bits.ftqIdx.value)
  ftb_entry_mem.io.raddr(redirectReadStart) := Mux(aheadValid, ftqIdxAhead(0).bits.value, backendRedirect.bits.ftqIdx.value)

  for (i <- 0 until FtqRedirectAheadNum) {
    ftq_redirect_rdata(i) := ftq_redirect_sram.io.rdata(i + redirectReadStart)
    ftb_redirect_rdata(i) := ftb_entry_mem.io.rdata(i + redirectReadStart)
  }
  val stage3CfiInfo = Mux(realAhdValid, Mux1H(ftqIdxSelOH, ftq_redirect_rdata), ftq_redirect_sram.io.rdata(redirectReadStart))
  val backendRedirectCfi = fromBackendRedirect.bits.cfiUpdate // use to report & resume GHR when redirect
  backendRedirectCfi.fromFtqRedirectSram(stage3CfiInfo)

  val r_ftb_entry = Mux(realAhdValid, Mux1H(ftqIdxSelOH, ftb_redirect_rdata), ftb_entry_mem.io.rdata(redirectReadStart))
  val r_ftqOffset = fromBackendRedirect.bits.ftqOffset

  backendRedirectCfi.br_hit := r_ftb_entry.brIsSaved(r_ftqOffset)
  backendRedirectCfi.jr_hit := r_ftb_entry.isJalr && r_ftb_entry.tailSlot.offset === r_ftqOffset
  // FIXME: not portable
  val sc_disagree = stage3CfiInfo.sc_disagree.getOrElse(VecInit(Seq.fill(numBr)(false.B))) // two sc pred result of two br instr
  // select sc result based on which br instr is real cfi
  backendRedirectCfi.sc_hit := backendRedirectCfi.br_hit && Mux(r_ftb_entry.brSlots(0).offset === r_ftqOffset,
    sc_disagree(0), sc_disagree(1))

  when(entry_hit_status(fromBackendRedirect.bits.ftqIdx.value) === h_hit) {
    backendRedirectCfi.shift := PopCount(r_ftb_entry.getBrMaskByOffset(r_ftqOffset)) +&
      (backendRedirectCfi.pd.isBr && !r_ftb_entry.brIsSaved(r_ftqOffset) &&
        !r_ftb_entry.newBrCanNotInsert(r_ftqOffset))

    backendRedirectCfi.addIntoHist := backendRedirectCfi.pd.isBr && (r_ftb_entry.brIsSaved(r_ftqOffset) ||
      !r_ftb_entry.newBrCanNotInsert(r_ftqOffset))
  }.otherwise {
    backendRedirectCfi.shift := (backendRedirectCfi.pd.isBr && backendRedirectCfi.taken).asUInt
    backendRedirectCfi.addIntoHist := backendRedirectCfi.pd.isBr.asUInt
  }

  /*****************************************************************************
  // **************************** redirect from ifu ****************************
  // *************************************************************************** */
  fromIfuRedirect.valid := iwb_p0_pdWb_valid && pdWb.bits.misOffset.valid && !backendFlush
  fromIfuRedirect.bits.ftqIdx := pdWb.bits.ftqIdx
  fromIfuRedirect.bits.ftqOffset := pdWb.bits.misOffset.bits
  fromIfuRedirect.bits.level := RedirectLevel.flushAfter
  fromIfuRedirect.bits.BTBMissBubble := true.B
  fromIfuRedirect.bits.debugIsMemVio := false.B
  fromIfuRedirect.bits.debugIsCtrl := false.B

  val ifuRedirectCfiUpdate = fromIfuRedirect.bits.cfiUpdate
  ifuRedirectCfiUpdate.pc := pdWb.bits.pc(pdWb.bits.misOffset.bits)
  ifuRedirectCfiUpdate.pd := pdWb.bits.pd(pdWb.bits.misOffset.bits)
  ifuRedirectCfiUpdate.predTaken := cfiIndex_vec(pdWb.bits.ftqIdx.value).valid
  ifuRedirectCfiUpdate.target := pdWb.bits.target
  ifuRedirectCfiUpdate.taken := pdWb.bits.cfiOffset.valid
  ifuRedirectCfiUpdate.isMisPred := pdWb.bits.misOffset.valid

  ftq_redirect_sram.io.ren.head := fromIfuRedirect.valid
  ftq_redirect_sram.io.raddr.head := fromIfuRedirect.bits.ftqIdx.value
  ftb_entry_mem.io.raddr.head := fromIfuRedirect.bits.ftqIdx.value

  val toBpuCfi = ifuRedirectToBpu.bits.cfiUpdate
  toBpuCfi.fromFtqRedirectSram(ftq_redirect_sram.io.rdata.head)
  when(ifuRedirectReg.bits.cfiUpdate.pd.isRet && ifuRedirectReg.bits.cfiUpdate.pd.valid) {
    toBpuCfi.target := toBpuCfi.topAddr
  }

  when(ifuRedirectReg.valid) {
    ifuRedirected(ifuRedirectReg.bits.ftqIdx.value) := true.B
  }.elsewhen(RegNext(iwb_p0_pdWb_valid)) {
    // if pdWb and no redirect, set to false
    ifuRedirected(b_p1_bpu_resp_idx) := false.B // TODO: ???why pdWb combined with bpu_resp_idx
  }

  // *********************************************************************
  // **************************** wb from exu ****************************
  // *********************************************************************
  backendRedirect.valid := io.fromBackend.redirect.valid
  backendRedirect.bits.connectRedirect(io.fromBackend.redirect.bits)
  backendRedirect.bits.BTBMissBubble := false.B

  def extractRedirectInfo(wb: Valid[Redirect]) = {
    val ftqPtr = wb.bits.ftqIdx
    val ftqOffset = wb.bits.ftqOffset
    val taken = wb.bits.cfiUpdate.taken
    val mispred = wb.bits.cfiUpdate.isMisPred
    (wb.valid, ftqPtr, ftqOffset, taken, mispred)
  }

  // fix mispredict entry
  val lastIsMispredict = RegNext(
    backendRedirect.valid && backendRedirect.bits.level === RedirectLevel.flushAfter, init = false.B
  )

  def updateCfiInfo(redirect: Valid[Redirect], isBackend: Boolean = true) = {
    val (r_valid, r_ptr, r_offset, r_taken, r_mispred) = extractRedirectInfo(redirect)
    val r_idx = r_ptr.value
    val cfiIndex_bits_wen = r_valid && r_taken && r_offset < cfiIndex_vec(r_idx).bits
    val cfiIndex_valid_wen = r_valid && r_offset === cfiIndex_vec(r_idx).bits
    when(cfiIndex_bits_wen || cfiIndex_valid_wen) {
      cfiIndex_vec(r_idx).valid := cfiIndex_bits_wen || cfiIndex_valid_wen && r_taken
    }.elsewhen(r_valid && !r_taken && r_offset =/= cfiIndex_vec(r_idx).bits) {
      cfiIndex_vec(r_idx).valid := false.B
    }
    when(cfiIndex_bits_wen) {
      cfiIndex_vec(r_idx).bits := r_offset
    }
    // redirect signal to BPU s2 stage
    b_p2_bpu_newest_entry_target := redirect.bits.cfiUpdate.target
    b_p2_bpu_newest_entry_ptr := r_ptr
    update_target(r_idx) := redirect.bits.cfiUpdate.target // TODO: remove this
    if (isBackend) {
      mispredict_vec(r_idx)(r_offset) := r_mispred
    }
  }

  when(fromBackendRedirect.valid) {
    updateCfiInfo(fromBackendRedirect)
  }.elsewhen(ifuRedirectToBpu.valid) {
    updateCfiInfo(ifuRedirectToBpu, isBackend = false)
  }

  when(fromBackendRedirect.valid) {
    when(fromBackendRedirect.bits.ControlRedirectBubble) {
      when(fromBackendRedirect.bits.ControlBTBMissBubble) {
        topdown_stage.reasons(TopDownCounters.BTBMissBubble.id) := true.B
        io.toIfu.req.bits.topdown_info.reasons(TopDownCounters.BTBMissBubble.id) := true.B
      }.elsewhen(fromBackendRedirect.bits.TAGEMissBubble) {
        topdown_stage.reasons(TopDownCounters.TAGEMissBubble.id) := true.B
        io.toIfu.req.bits.topdown_info.reasons(TopDownCounters.TAGEMissBubble.id) := true.B
      }.elsewhen(fromBackendRedirect.bits.SCMissBubble) {
        topdown_stage.reasons(TopDownCounters.SCMissBubble.id) := true.B
        io.toIfu.req.bits.topdown_info.reasons(TopDownCounters.SCMissBubble.id) := true.B
      }.elsewhen(fromBackendRedirect.bits.ITTAGEMissBubble) {
        topdown_stage.reasons(TopDownCounters.ITTAGEMissBubble.id) := true.B
        io.toIfu.req.bits.topdown_info.reasons(TopDownCounters.ITTAGEMissBubble.id) := true.B
      }.elsewhen(fromBackendRedirect.bits.RASMissBubble) {
        topdown_stage.reasons(TopDownCounters.RASMissBubble.id) := true.B
        io.toIfu.req.bits.topdown_info.reasons(TopDownCounters.RASMissBubble.id) := true.B
      }

    }.elsewhen(backendRedirect.bits.MemVioRedirectBubble) {
      topdown_stage.reasons(TopDownCounters.MemVioRedirectBubble.id) := true.B
      io.toIfu.req.bits.topdown_info.reasons(TopDownCounters.MemVioRedirectBubble.id) := true.B
    }.otherwise {
      topdown_stage.reasons(TopDownCounters.OtherRedirectBubble.id) := true.B
      io.toIfu.req.bits.topdown_info.reasons(TopDownCounters.OtherRedirectBubble.id) := true.B
    }
  }.elsewhen(ifuRedirectReg.valid) {
    topdown_stage.reasons(TopDownCounters.BTBMissBubble.id) := true.B
    io.toIfu.req.bits.topdown_info.reasons(TopDownCounters.BTBMissBubble.id) := true.B
  }

  io.ControlBTBMissBubble := fromBackendRedirect.bits.ControlBTBMissBubble
  io.TAGEMissBubble := fromBackendRedirect.bits.TAGEMissBubble
  io.SCMissBubble := fromBackendRedirect.bits.SCMissBubble
  io.ITTAGEMissBubble := fromBackendRedirect.bits.ITTAGEMissBubble
  io.RASMissBubble := fromBackendRedirect.bits.RASMissBubble


  /** ************************** redirect to ifu **************************** */
  // only the valid bit is actually needed
  io.toIfu.redirect.bits := backendRedirect.bits
  io.toIfu.redirect.valid := stage2Flush
  io.toIfu.topdown_redirect := fromBackendRedirect

  /** ************************** redirect to bpu **************************** */
  io.toBpu.redirect := Mux(fromBackendRedirect.valid, fromBackendRedirect, ifuRedirectToBpu)
  val dummy_s1_pred_cycle_vec = VecInit(List.tabulate(FtqSize)(_ => 0.U(64.W)))
  val redirect_latency = GTimer() - pred_s1_cycle.getOrElse(dummy_s1_pred_cycle_vec)(io.toBpu.redirect.bits.ftqIdx.value) + 1.U
  XSPerfHistogram("backend_redirect_latency", redirect_latency, fromBackendRedirect.valid, 0, 60, 1)
  XSPerfHistogram("ifu_redirect_latency", redirect_latency, !fromBackendRedirect.valid && ifuRedirectToBpu.valid, 0, 60, 1)
  XSError(io.toBpu.redirect.valid && isBefore(io.toBpu.redirect.bits.ftqIdx, commPtr), "Ftq received a redirect after its commit, check backend or replay")

  /** ************************** record ROB commit **************************** */
  // commit a instr corresponding a bit of the item of commitStateQueue
  // (indexed by ftqOffset & ftqIdx respectively) turn to commited, and fusion instr will
  // mark multi-bit commited. When all bits of a item are commited or invalid(ignored in instr block)
  // the instr block corresonding to item can be commited
  for (c <- io.fromBackend.rob_commits) {
    when(c.valid) {
      commitStateQueue(c.bits.ftqIdx.value)(c.bits.ftqOffset) := c_commited
      // TODO: remove this
      // For instruction fusions, we also update the next instruction
      when(c.bits.commitType === 4.U) {
        commitStateQueue(c.bits.ftqIdx.value)(c.bits.ftqOffset + 1.U) := c_commited
      }.elsewhen(c.bits.commitType === 5.U) {
        commitStateQueue(c.bits.ftqIdx.value)(c.bits.ftqOffset + 2.U) := c_commited
      }.elsewhen(c.bits.commitType === 6.U) {
        val index = (c.bits.ftqIdx + 1.U).value
        commitStateQueue(index)(0) := c_commited
      }.elsewhen(c.bits.commitType === 7.U) {
        val index = (c.bits.ftqIdx + 1.U).value
        commitStateQueue(index)(1) := c_commited
      }
    }
  }

  /** *************************************************************************** */
  /** ************************** commit & update BPU **************************** */
  /** *************************************************************************** */
  val bpu_not_in_stall = Wire(Bool())
  val bpu_ftb_update_stall = RegInit(0.U(2.W)) // 2-cycle stall, so we need 3 states
  bpu_not_in_stall := bpu_ftb_update_stall === 0.U
  canCommit := commPtr =/= ifuWbPtr && bpu_not_in_stall &&
    Cat(commitStateQueue(commPtr.value).map(s => {
      s === c_invalid || s === c_commited
    })).andR // each item in commitStateQueue indicates a inst block, which has PredictWidth-bit states representing PredictWidth instr in a block,
             // whether a queue item canCommit depends on the state of eah instr:
             //  if a instr state is invalid, it means that it does not need to be considered
             //  if a instr state is valid, it must wait it to be commited state.

  val mmioReadPtr = io.mmioCommitRead.mmioFtqPtr
  val mmioLastCommit = isBefore(commPtr, mmioReadPtr) && (isAfter(ifuPtr, mmioReadPtr) || mmioReadPtr === ifuPtr) &&
    Cat(commitStateQueue(mmioReadPtr.value).map(s => {
      s === c_invalid || s === c_commited
    })).andR
  io.mmioCommitRead.mmioLastCommit := RegNext(mmioLastCommit)

  // commit reads
  ftq_pc_mem.io.commPtr_n := commPtr_next
  ftq_pc_mem.io.commPtrPlus1_n := commPtrPlus1_next
  val commit_pc_bundle = RegNext(ftq_pc_mem.io.commPtr_rdata)
  val commit_target =
    Mux(RegNext(commPtr === b_p2_bpu_newest_entry_ptr),
      RegNext(b_p2_bpu_newest_entry_target),
      RegNext(ftq_pc_mem.io.commPtrPlus1_rdata.startAddr))

  ftq_pd_mem.io.raddr.last := commPtr.value
  val commit_pd = ftq_pd_mem.io.rdata.last

  ftq_redirect_sram.io.ren.last := canCommit
  ftq_redirect_sram.io.raddr.last := commPtr.value
  val commit_spec_info = ftq_redirect_sram.io.rdata.last

  ftq_meta_1r_sram.io.ren(0) := canCommit
  ftq_meta_1r_sram.io.raddr(0) := commPtr.value
  val commit_meta = ftq_meta_1r_sram.io.rdata(0)

  ftb_entry_mem.io.raddr.last := commPtr.value
  val commit_ftb_entry = ftb_entry_mem.io.rdata.last

  // need one cycle to read mem and srams
  val do_commit_ptr = RegNext(commPtr)
  val do_commit = RegNext(canCommit, init = false.B)
  when(canCommit) {
    commPtr_next := commPtrPlus1
    commPtrPlus1_next := commPtrPlus1 + 1.U
  }
  val commit_state = RegNext(commitStateQueue(commPtr.value))
  val can_commit_cfi = WireInit(cfiIndex_vec(commPtr.value))
  val do_commit_cfi = WireInit(cfiIndex_vec(do_commit_ptr.value))
  //
  //when (commitStateQueue(commPtr.value)(can_commit_cfi.bits) =/= c_commited) {
  //  can_commit_cfi.valid := false.B
  //}
  val commit_cfi = RegNext(can_commit_cfi)
  val debug_cfi = commitStateQueue(do_commit_ptr.value)(do_commit_cfi.bits) =/= c_commited && do_commit_cfi.valid

  val commit_mispredict: Vec[Bool] = VecInit((RegNext(mispredict_vec(commPtr.value)) zip commit_state).map {
    case (mis, state) => mis && state === c_commited
  })
  val commit_instCommited: Vec[Bool] = VecInit(commit_state.map(_ === c_commited)) // [PredictWidth]
  val can_commit_hit = entry_hit_status(commPtr.value)
  val commit_hit = RegNext(can_commit_hit)
  val diff_commit_target = RegNext(update_target(commPtr.value)) // TODO: remove this
  val commit_stage = RegNext(pred_stage(commPtr.value))
  val commit_valid = commit_hit === h_hit || commit_cfi.valid // hit or taken

  val to_bpu_hit = can_commit_hit === h_hit || can_commit_hit === h_false_hit
  switch(bpu_ftb_update_stall) {
    is(0.U) {
      when(can_commit_cfi.valid && !to_bpu_hit && canCommit) {
        bpu_ftb_update_stall := 2.U // 2-cycle stall
      }
    }
    is(2.U) {
      bpu_ftb_update_stall := 1.U
    }
    is(1.U) {
      bpu_ftb_update_stall := 0.U
    }
    is(3.U) {
      XSError(true.B, "bpu_ftb_update_stall should be 0, 1 or 2")
    }
  }

  // TODO: remove this
  XSError(do_commit && diff_commit_target =/= commit_target, "\ncommit target should be the same as update target\n")

  // update latency stats
  val update_latency = GTimer() - pred_s1_cycle.getOrElse(dummy_s1_pred_cycle_vec)(do_commit_ptr.value) + 1.U
  XSPerfHistogram("bpu_update_latency", update_latency, io.toBpu.update.valid, 0, 64, 2)

  /** ********** use Storage info to generate BPU update & FTB entry update info ********** */
  io.toBpu.update := DontCare
  io.toBpu.update.valid := commit_valid && do_commit
  val update = io.toBpu.update.bits
  update.false_hit := commit_hit === h_false_hit
  update.pc := commit_pc_bundle.startAddr
  update.meta := commit_meta.meta
  update.cfi_idx := commit_cfi
  update.full_target := commit_target
  update.from_stage := commit_stage
  update.spec_info := commit_spec_info
  XSError(commit_valid && do_commit && debug_cfi, "\ncommit cfi can be non c_commited\n")

  val commit_real_hit = commit_hit === h_hit

  val ftbEntryGen = Module(new FTBEntryGen).io
  ftbEntryGen.start_addr := commit_pc_bundle.startAddr
  ftbEntryGen.old_entry := commit_ftb_entry
  ftbEntryGen.pd := commit_pd
  ftbEntryGen.cfiIndex := commit_cfi
  ftbEntryGen.target := commit_target
  ftbEntryGen.hit := commit_real_hit
  ftbEntryGen.mispredict_vec := commit_mispredict

  val update_ftb_entry = update.ftb_entry
  update_ftb_entry := ftbEntryGen.new_entry
  update.new_br_insert_pos := ftbEntryGen.new_br_insert_pos
  update.mispred_mask := ftbEntryGen.mispred_mask
  update.old_entry := ftbEntryGen.is_old_entry
  update.pred_hit := commit_hit === h_hit || commit_hit === h_false_hit
  update.br_taken_mask := ftbEntryGen.taken_mask
  update.br_committed := (ftbEntryGen.new_entry.brValids zip ftbEntryGen.new_entry.brOffset) map {
    case (valid, offset) => valid && commit_instCommited(offset)
  }
  update.jmp_taken := ftbEntryGen.jmp_taken

  // update.full_pred.fromFtbEntry(ftbEntryGen.new_entry, update.pc)
  // update.full_pred.jalr_target := commit_target
  // update.full_pred.hit := true.B
  // when (update.full_pred.is_jalr) {
  //   update.full_pred.targets.last := commit_target
  // }

  // ****************************************************************
  // *********************** to prefetch ****************************
  // ****************************************************************
  /**
   * *****************************************************************************
   * prefetchPtr control
   * - 1. prefetchPtr plus 1 when toPrefetch fire and keep distance from bpuPtr more than 2
   * - 2. limit range of prefetchPtr is in [ifuPtr + minRange, ifuPtr + maxRange]
   * - 3. flush prefetchPtr when receive redirect from ifu or backend
   * *****************************************************************************
   */
  val prefetchPtr = RegInit(FtqPtr(false.B, 0.U))
  val nextPrefetchPtr = WireInit(prefetchPtr)
  prefetchPtr := nextPrefetchPtr

  // ************************ Prefetch Ptr Update ***************************
  when(redirectVec.map(r => r.valid).reduce(_ || _)) { // redirect resume
    nextPrefetchPtr := r_sel_idx + minRangeFromIFUptr.U
  }.elsewhen(prefetchPtr < ifuPtr + minRangeFromIFUptr.U) { // limit range of prefetchPtr from ifuPtr
    nextPrefetchPtr := ifuPtr + minRangeFromIFUptr.U
  }.elsewhen(prefetchPtr > ifuPtr + maxRangeFromIFUptr.U) {
    nextPrefetchPtr := ifuPtr + maxRangeFromIFUptr.U
  }.elsewhen(io.toPrefetch.req.fire) { // increase when toPrefetch fire
    when(prefetchPtr < bpuPtr - 2.U) { // TODO: consider req which cross cacheline
      nextPrefetchPtr := prefetchPtr + 1.U
    }
  }

  // data from ftq_pc_mem has 1 cycle delay
  io.toPrefetch.req.valid := RegNext(entry_fetch_status(nextPrefetchPtr.value) === f_to_send)
  ftq_pc_mem.io.other_raddrs(0) := nextPrefetchPtr.value
  io.toPrefetch.req.bits.target := ftq_pc_mem.io.other_rdatas(0).startAddr

  // record position relationship between ifuPtr, pfPtr and bpuPtr
  val isWritePrefetchPtrTable = WireInit(Constantin.createRecord("isWritePrefetchPtrTable" + HartId.toString))
  val prefetchPtrTable = ChiselDB.createTable("PrefetchPtrTable" + HartId.toString, new PrefetchPtrDB)
  val prefetchPtrDumpData = Wire(new PrefetchPtrDB)
  prefetchPtrDumpData.fromBpuPtr := distanceBetween(bpuPtr, prefetchPtr)
  prefetchPtrDumpData.fromIfuPtr := distanceBetween(prefetchPtr, ifuPtr)

  prefetchPtrTable.log(
    data = prefetchPtrDumpData,
    en = isWritePrefetchPtrTable.orR && io.toPrefetch.req.fire,
    site = "FTQ" + HartId.toString,
    clock = clock,
    reset = reset
  )


  /** *****************************************************************************
  // **************************** commit perf counters ****************************
  // ****************************************************************************** */
  val commit_inst_mask = VecInit(commit_state.map(c => c === c_commited && do_commit)).asUInt
  val commit_mispred_mask = commit_mispredict.asUInt
  val commit_not_mispred_mask = (~commit_mispred_mask).asUInt

  val commit_br_mask = commit_pd.brMask.asUInt
  val commit_jmp_mask = UIntToOH(commit_pd.jmpOffset) & Fill(PredictWidth, commit_pd.jmpInfo.valid.asTypeOf(UInt(1.W)))
  val commit_cfi_mask = (commit_br_mask | commit_jmp_mask)

  val mbpInstrs = commit_inst_mask & commit_cfi_mask

  val mbpRights = mbpInstrs & commit_not_mispred_mask
  val mbpWrongs = mbpInstrs & commit_mispred_mask

  io.bpuInfo.bpRight := PopCount(mbpRights)
  io.bpuInfo.bpWrong := PopCount(mbpWrongs)

  val isWriteFTQTable = WireInit(Constantin.createRecord("isWriteFTQTable" + HartId.toString))
  val ftqBranchTraceDB = ChiselDB.createTable("FTQTable" + HartId.toString, new FtqDebugBundle)
  // Cfi Info
  for (i <- 0 until PredictWidth) {
    val pc = commit_pc_bundle.startAddr + (i * instBytes).U
    val v = commit_state(i) === c_commited
    val isBr = commit_pd.brMask(i)
    val isJmp = commit_pd.jmpInfo.valid && commit_pd.jmpOffset === i.U
    val isCfi = isBr || isJmp
    val isTaken = commit_cfi.valid && commit_cfi.bits === i.U
    val misPred = commit_mispredict(i)
    // val ghist = commit_spec_info.ghist.predHist
    val histPtr = commit_spec_info.histPtr
    val predCycle = commit_meta.meta(63, 0)
    val target = commit_target

    val brIdx = OHToUInt(Reverse(Cat(update_ftb_entry.brValids.zip(update_ftb_entry.brOffset).map { case (v, offset) => v && offset === i.U })))
    val inFtbEntry = update_ftb_entry.brValids.zip(update_ftb_entry.brOffset).map { case (v, offset) => v && offset === i.U }.reduce(_ || _)
    val addIntoHist = ((commit_hit === h_hit) && inFtbEntry) || ((!(commit_hit === h_hit) && i.U === commit_cfi.bits && isBr && commit_cfi.valid))
    XSDebug(v && do_commit && isCfi, p"cfi_update: isBr(${isBr}) pc(${Hexadecimal(pc)}) " +
      p"taken(${isTaken}) mispred(${misPred}) cycle($predCycle) hist(${histPtr.value}) " +
      p"startAddr(${Hexadecimal(commit_pc_bundle.startAddr)}) AddIntoHist(${addIntoHist}) " +
      p"brInEntry(${inFtbEntry}) brIdx(${brIdx}) target(${Hexadecimal(target)})\n")

    val logbundle = Wire(new FtqDebugBundle)
    logbundle.pc := pc
    logbundle.target := target
    logbundle.isBr := isBr
    logbundle.isJmp := isJmp
    logbundle.isCall := isJmp && commit_pd.hasCall
    logbundle.isRet := isJmp && commit_pd.hasRet
    logbundle.misPred := misPred
    logbundle.isTaken := isTaken
    logbundle.predStage := commit_stage

    ftqBranchTraceDB.log(
      data = logbundle /* hardware of type T */ ,
      en = isWriteFTQTable.orR && v && do_commit && isCfi,
      site = "FTQ" + HartId.toString,
      clock = clock,
      reset = reset
    )
  }

  val enq = io.fromBpu.resp
  val perf_redirect = backendRedirect

  XSPerfAccumulate("entry", validEntries)
  XSPerfAccumulate("bpu_to_ftq_stall", enq.valid && !enq.ready)
  XSPerfAccumulate("mispredictRedirect", perf_redirect.valid && RedirectLevel.flushAfter === perf_redirect.bits.level)
  XSPerfAccumulate("replayRedirect", perf_redirect.valid && RedirectLevel.flushItself(perf_redirect.bits.level))
  XSPerfAccumulate("predecodeRedirect", fromIfuRedirect.valid)

  XSPerfAccumulate("to_ifu_bubble", io.toIfu.req.ready && !io.toIfu.req.valid)

  XSPerfAccumulate("to_ifu_stall", io.toIfu.req.valid && !io.toIfu.req.ready)
  XSPerfAccumulate("from_bpu_real_bubble", !enq.valid && enq.ready && allowBpuIn)
  XSPerfAccumulate("bpu_to_ifu_bubble", bpuPtr === ifuPtr)
  XSPerfAccumulate("bpu_to_ifu_bubble_when_ftq_full", (bpuPtr === ifuPtr) && isFull(bpuPtr, commPtr) && io.toIfu.req.ready)

  XSPerfAccumulate("redirectAhead_ValidNum", ftqIdxAhead.map(_.valid).reduce(_ | _))
  XSPerfAccumulate("fromBackendRedirect_ValidNum", io.fromBackend.redirect.valid)
  XSPerfAccumulate("toBpuRedirect_ValidNum", io.toBpu.redirect.valid)

  val from_bpu = io.fromBpu.resp.bits
  val to_ifu = io.toIfu.req.bits


  XSPerfHistogram("commit_num_inst", PopCount(commit_inst_mask), do_commit, 0, PredictWidth + 1, 1)


  val commit_jal_mask = UIntToOH(commit_pd.jmpOffset) & Fill(PredictWidth, commit_pd.hasJal.asTypeOf(UInt(1.W)))
  val commit_jalr_mask = UIntToOH(commit_pd.jmpOffset) & Fill(PredictWidth, commit_pd.hasJalr.asTypeOf(UInt(1.W)))
  val commit_call_mask = UIntToOH(commit_pd.jmpOffset) & Fill(PredictWidth, commit_pd.hasCall.asTypeOf(UInt(1.W)))
  val commit_ret_mask = UIntToOH(commit_pd.jmpOffset) & Fill(PredictWidth, commit_pd.hasRet.asTypeOf(UInt(1.W)))


  val mbpBRights = mbpRights & commit_br_mask
  val mbpJRights = mbpRights & commit_jal_mask
  val mbpIRights = mbpRights & commit_jalr_mask
  val mbpCRights = mbpRights & commit_call_mask
  val mbpRRights = mbpRights & commit_ret_mask

  val mbpBWrongs = mbpWrongs & commit_br_mask
  val mbpJWrongs = mbpWrongs & commit_jal_mask
  val mbpIWrongs = mbpWrongs & commit_jalr_mask
  val mbpCWrongs = mbpWrongs & commit_call_mask
  val mbpRWrongs = mbpWrongs & commit_ret_mask

  val commit_pred_stage = RegNext(pred_stage(commPtr.value))

  def pred_stage_map(src: UInt, name: String) = {
    (0 until numBpStages).map(i =>
      f"${name}_stage_${i + 1}" -> PopCount(src.asBools.map(_ && commit_pred_stage === BP_STAGES(i)))
    ).foldLeft(Map[String, UInt]())(_ + _)
  }

  val mispred_stage_map = pred_stage_map(mbpWrongs, "mispredict")
  val br_mispred_stage_map = pred_stage_map(mbpBWrongs, "br_mispredict")
  val jalr_mispred_stage_map = pred_stage_map(mbpIWrongs, "jalr_mispredict")
  val correct_stage_map = pred_stage_map(mbpRights, "correct")
  val br_correct_stage_map = pred_stage_map(mbpBRights, "br_correct")
  val jalr_correct_stage_map = pred_stage_map(mbpIRights, "jalr_correct")

  val update_valid = io.toBpu.update.valid

  def u(cond: Bool) = update_valid && cond

  val ftb_false_hit = u(update.false_hit)
  // assert(!ftb_false_hit)
  val ftb_hit = u(commit_hit === h_hit)

  val ftb_new_entry = u(ftbEntryGen.is_init_entry)
  val ftb_new_entry_only_br = ftb_new_entry && !update_ftb_entry.jmpValid
  val ftb_new_entry_only_jmp = ftb_new_entry && !update_ftb_entry.brValids(0)
  val ftb_new_entry_has_br_and_jmp = ftb_new_entry && update_ftb_entry.brValids(0) && update_ftb_entry.jmpValid

  val ftb_old_entry = u(ftbEntryGen.is_old_entry)

  val ftb_modified_entry = u(ftbEntryGen.is_new_br || ftbEntryGen.is_jalr_target_modified || ftbEntryGen.is_always_taken_modified)
  val ftb_modified_entry_new_br = u(ftbEntryGen.is_new_br)
  val ftb_modified_entry_ifu_redirected = u(ifuRedirected(do_commit_ptr.value))
  val ftb_modified_entry_jalr_target_modified = u(ftbEntryGen.is_jalr_target_modified)
  val ftb_modified_entry_br_full = ftb_modified_entry && ftbEntryGen.is_br_full
  val ftb_modified_entry_always_taken = ftb_modified_entry && ftbEntryGen.is_always_taken_modified

  def getFtbEntryLen(pc: UInt, entry: FTBEntry) = ((entry.getFallThrough(pc) - pc) >> instOffsetBits).asUInt

  val gen_ftb_entry_len = getFtbEntryLen(update.pc, ftbEntryGen.new_entry)
  XSPerfHistogram("ftb_init_entry_len", gen_ftb_entry_len, ftb_new_entry, 0, PredictWidth + 1, 1)
  XSPerfHistogram("ftb_modified_entry_len", gen_ftb_entry_len, ftb_modified_entry, 0, PredictWidth + 1, 1)
  val s3_ftb_entry_len = getFtbEntryLen(from_bpu.s3.pc(0), from_bpu.last_stage_ftb_entry)
  XSPerfHistogram("s3_ftb_entry_len", s3_ftb_entry_len, from_bpu.s3.valid(0), 0, PredictWidth + 1, 1)

  XSPerfHistogram("ftq_has_entry", validEntries, true.B, 0, FtqSize + 1, 1)

  val perfCountsMap = Map(
    "BpInstr" -> PopCount(mbpInstrs),
    "BpBInstr" -> PopCount(mbpBRights | mbpBWrongs),
    "BpRight" -> PopCount(mbpRights),
    "BpWrong" -> PopCount(mbpWrongs),
    "BpBRight" -> PopCount(mbpBRights),
    "BpBWrong" -> PopCount(mbpBWrongs),
    "BpJRight" -> PopCount(mbpJRights),
    "BpJWrong" -> PopCount(mbpJWrongs),
    "BpIRight" -> PopCount(mbpIRights),
    "BpIWrong" -> PopCount(mbpIWrongs),
    "BpCRight" -> PopCount(mbpCRights),
    "BpCWrong" -> PopCount(mbpCWrongs),
    "BpRRight" -> PopCount(mbpRRights),
    "BpRWrong" -> PopCount(mbpRWrongs),

    "ftb_false_hit" -> PopCount(ftb_false_hit),
    "ftb_hit" -> PopCount(ftb_hit),
    "ftb_new_entry" -> PopCount(ftb_new_entry),
    "ftb_new_entry_only_br" -> PopCount(ftb_new_entry_only_br),
    "ftb_new_entry_only_jmp" -> PopCount(ftb_new_entry_only_jmp),
    "ftb_new_entry_has_br_and_jmp" -> PopCount(ftb_new_entry_has_br_and_jmp),
    "ftb_old_entry" -> PopCount(ftb_old_entry),
    "ftb_modified_entry" -> PopCount(ftb_modified_entry),
    "ftb_modified_entry_new_br" -> PopCount(ftb_modified_entry_new_br),
    "ftb_jalr_target_modified" -> PopCount(ftb_modified_entry_jalr_target_modified),
    "ftb_modified_entry_br_full" -> PopCount(ftb_modified_entry_br_full),
    "ftb_modified_entry_always_taken" -> PopCount(ftb_modified_entry_always_taken)
  ) ++ mispred_stage_map ++ br_mispred_stage_map ++ jalr_mispred_stage_map ++
    correct_stage_map ++ br_correct_stage_map ++ jalr_correct_stage_map

  for ((key, value) <- perfCountsMap) {
    XSPerfAccumulate(key, value)
  }

  // --------------------------- Debug --------------------------------
  // XSDebug(b_p0_bpu_enq_fire, p"enq! " + io.fromBpu.resp.bits.toPrintable)
  XSDebug(io.toIfu.req.fire, p"fire to ifu " + io.toIfu.req.bits.toPrintable)
  XSDebug(do_commit, p"deq! [ptr] $do_commit_ptr\n")
  XSDebug(true.B, p"[bpuPtr] $bpuPtr, [ifuPtr] $ifuPtr, [ifuWbPtr] $ifuWbPtr [commPtr] $commPtr\n")
  XSDebug(true.B, p"[in] v:${io.fromBpu.resp.valid} r:${io.fromBpu.resp.ready} " +
    p"[out] v:${io.toIfu.req.valid} r:${io.toIfu.req.ready}\n")
  XSDebug(do_commit, p"[deq info] cfiIndex: $commit_cfi, $commit_pc_bundle, target: ${Hexadecimal(commit_target)}\n")

  val perfEvents = Seq(
    ("bpu_s2_redirect        ", bpu_s2_redirect),
    ("bpu_s3_redirect        ", bpu_s3_redirect),
    ("bpu_to_ftq_stall       ", enq.valid && (~enq.ready).asBool),
    ("mispredictRedirect     ", perf_redirect.valid && RedirectLevel.flushAfter === perf_redirect.bits.level),
    ("replayRedirect         ", perf_redirect.valid && RedirectLevel.flushItself(perf_redirect.bits.level)),
    ("predecodeRedirect      ", fromIfuRedirect.valid),
    ("to_ifu_bubble          ", io.toIfu.req.ready && !io.toIfu.req.valid),
    ("from_bpu_real_bubble   ", !enq.valid && enq.ready && allowBpuIn),
    ("BpInstr                ", PopCount(mbpInstrs)),
    ("BpBInstr               ", PopCount(mbpBRights | mbpBWrongs)),
    ("BpRight                ", PopCount(mbpRights)),
    ("BpWrong                ", PopCount(mbpWrongs)),
    ("BpBRight               ", PopCount(mbpBRights)),
    ("BpBWrong               ", PopCount(mbpBWrongs)),
    ("BpJRight               ", PopCount(mbpJRights)),
    ("BpJWrong               ", PopCount(mbpJWrongs)),
    ("BpIRight               ", PopCount(mbpIRights)),
    ("BpIWrong               ", PopCount(mbpIWrongs)),
    ("BpCRight               ", PopCount(mbpCRights)),
    ("BpCWrong               ", PopCount(mbpCWrongs)),
    ("BpRRight               ", PopCount(mbpRRights)),
    ("BpRWrong               ", PopCount(mbpRWrongs)),
    ("ftb_false_hit          ", PopCount(ftb_false_hit)),
    ("ftb_hit                ", PopCount(ftb_hit)),
  )
  generatePerfEvent()
}
