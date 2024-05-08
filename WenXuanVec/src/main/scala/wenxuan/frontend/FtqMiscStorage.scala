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

/**
 * Store Prediction PC Address from BPU and Used by ICache fetch and IFU predecode & issue,
 *  IPrefetch prefetch instr and commit & update BPU
 */
class Ftq_RF_Components(implicit p: Parameters) extends WXBundle with HasFtqParameters
{
  val startAddr = UInt(VAddrBits.W)
  val nextLineAddr = UInt(VAddrBits.W)
  val isNextMask = Vec(PredictWidth, Bool())
  val fallThruError = Bool()

  def fromBranchPrediction(pred: BranchPredictionBundle) = {
    this.startAddr := pred.pc(3)
    // (FetchWidth * 4 * 2).U : when fetch width(the number of instrs) * 4-byte (default instr width)
    //  is half of the blockBytes, FetchWidth * 4 * 2 = blockBytes to get the nextLineAddr,
    // so comment "may be broken on other configs" in XiangShan
    // in Wenxuan, fetch width * 4 must be half of the blockBytes, so just write "+ blockBytes.U“ to get the nextLineAddr
    this.nextLineAddr := pred.pc(3) + blockBytes.U
    // Assume that each instr is a 2-byte Compress instr, calculate whether
    //  the start address of each instr is within the PredictWidth range of startAddr
    // if no in the range, the bit in the log2Ceil(PredictWidth) is 1, the instr is belong to next
    this.isNextMask := VecInit((0 until PredictWidth).map(i =>
      (pred.pc(3)(log2Ceil(PredictWidth), 1) +& i.U)(log2Ceil(PredictWidth)).asBool
    ))
    this.fallThruError := pred.fallThruError(3)
    this
  }
}

class FtqPcMemWrapper(numOtherReads: Int)(implicit p: Parameters) extends WXModule {
  val io = IO(new Bundle {
    val ifuPtr_n       = Input(new FtqPtr)
    val ifuPtrPlus1_n  = Input(new FtqPtr)
    val ifuPtrPlus2_n  = Input(new FtqPtr)
    val commPtr_n      = Input(new FtqPtr)
    val commPtrPlus1_n = Input(new FtqPtr)
    val ifuPtr_rdata       = Output(new Ftq_RF_Components)
    val ifuPtrPlus1_rdata  = Output(new Ftq_RF_Components)
    val ifuPtrPlus2_rdata  = Output(new Ftq_RF_Components)
    val commPtr_rdata      = Output(new Ftq_RF_Components)
    val commPtrPlus1_rdata = Output(new Ftq_RF_Components)

    val other_raddrs = Input(Vec(numOtherReads, UInt(log2Ceil(FtqSize).W)))
    val other_rdatas = Output(Vec(numOtherReads, new Ftq_RF_Components))

    val wen = Input(Bool())
    val waddr = Input(UInt(log2Ceil(FtqSize).W))
    val wdata = Input(new Ftq_RF_Components)
  })

  val num_pc_read = numOtherReads + 5
  val mem = Module(new SyncDataModuleTemplate(new Ftq_RF_Components, FtqSize,
    num_pc_read, 1, "FtqPC"))
  mem.io.wen(0)   := io.wen
  mem.io.waddr(0) := io.waddr
  mem.io.wdata(0) := io.wdata

  // read one cycle ahead for ftq local reads
  val raddr_vec = VecInit(io.other_raddrs ++
    Seq(io.ifuPtr_n.value, io.ifuPtrPlus1_n.value, io.ifuPtrPlus2_n.value, io.commPtrPlus1_n.value, io.commPtr_n.value))

  mem.io.raddr := raddr_vec

  io.other_rdatas       := mem.io.rdata.dropRight(5)
  io.ifuPtr_rdata       := mem.io.rdata.dropRight(4).last
  io.ifuPtrPlus1_rdata  := mem.io.rdata.dropRight(3).last
  io.ifuPtrPlus2_rdata  := mem.io.rdata.dropRight(2).last
  io.commPtrPlus1_rdata := mem.io.rdata.dropRight(1).last
  io.commPtr_rdata      := mem.io.rdata.last
}

/**
 * Store predecode jump/branch information from IFU and Used by Update BPU and performance counter
 */
class Ftq_pd_Entry(implicit p: Parameters) extends WXBundle {
  val brMask = Vec(PredictWidth, Bool()) // whether each instr is conditional branch instr
  val jmpInfo = ValidUndirectioned(Vec(3, Bool())) // jalr, call, ret
  val jmpOffset = UInt(log2Ceil(PredictWidth).W) // position of unconditional j instr in instr block
  val jalTarget = UInt(VAddrBits.W) // predecode output jal target
  val rvcMask = Vec(PredictWidth, Bool()) // whether each instr is compress instr
  def hasJal  = jmpInfo.valid && !jmpInfo.bits(0)
  def hasJalr = jmpInfo.valid && jmpInfo.bits(0)
  def hasCall = jmpInfo.valid && jmpInfo.bits(1)
  def hasRet  = jmpInfo.valid && jmpInfo.bits(2)

  def fromPdWb(pdWb: PredecodeWritebackBundle) = {
    val pds = pdWb.pd
    this.brMask := VecInit(pds.map(pd => pd.isBr && pd.valid))
    this.jmpInfo.valid := VecInit(pds.map(pd => (pd.isJal || pd.isJalr) && pd.valid)).asUInt.orR
    this.jmpInfo.bits := ParallelPriorityMux(pds.map(pd => (pd.isJal || pd.isJalr) && pd.valid),
      pds.map(pd => VecInit(pd.isJalr, pd.isCall, pd.isRet)))
    // position of the first unconditional jump instr in the instr block
    this.jmpOffset := ParallelPriorityEncoder(pds.map(pd => (pd.isJal || pd.isJalr) && pd.valid))
    this.rvcMask := VecInit(pds.map(pd => pd.isRVC))
    this.jalTarget := pdWb.jalTarget
  }

  def toPd(offset: UInt) = {
    require(offset.getWidth == log2Ceil(PredictWidth))
    val pd = Wire(new PreDecodeInfo)
    pd.valid := true.B
    pd.isRVC := rvcMask(offset)
    val isBr = brMask(offset)
    val isJalr = offset === jmpOffset && jmpInfo.valid && jmpInfo.bits(0)
    pd.brType := Cat(offset === jmpOffset && jmpInfo.valid, isJalr || isBr)
    pd.isCall := offset === jmpOffset && jmpInfo.valid && jmpInfo.bits(1)
    pd.isRet  := offset === jmpOffset && jmpInfo.valid && jmpInfo.bits(2)
    pd
  }
}

/** SRAM storage structure used in FTQ */
class FtqNRSRAM[T <: Data](gen: T, numRead: Int)(implicit p: Parameters) extends WXModule {

  val io = IO(new Bundle() {
    val raddr = Input(Vec(numRead, UInt(log2Up(FtqSize).W)))
    val ren = Input(Vec(numRead, Bool()))
    val rdata = Output(Vec(numRead, gen))
    val waddr = Input(UInt(log2Up(FtqSize).W))
    val wen = Input(Bool())
    val wdata = Input(gen)
  })

  for(i <- 0 until numRead){
    val sram = Module(new SRAMTemplate(gen, FtqSize))
    sram.io.r.req.valid := io.ren(i)
    sram.io.r.req.bits.setIdx := io.raddr(i)
    io.rdata(i) := sram.io.r.resp.data(0)
    sram.io.w.req.valid := io.wen
    sram.io.w.req.bits.setIdx := io.waddr
    sram.io.w.req.bits.data := VecInit(io.wdata)
  }
}

/** Temporarily store the information about the speculative updated BPU structure,
 *  like RAS & Global history, used to resume these BPU structure information when redirect */
class Ftq_Redirect_SRAMEntry(implicit p: Parameters) extends SpeculativeInfo {
  val sc_disagree = if (!env.FPGAPlatform) Some(Vec(numBr, Bool())) else None
}

/** store *last_stage_meta* concatenated from all meta info output by each BPU, like TAGE tag, etc.
 *  used to resume the meta info when redirect*/
class Ftq_1R_SRAMEntry(implicit p: Parameters) extends WXBundle with HasBPUConst {
  val meta = UInt(MaxMetaLength.W)
}