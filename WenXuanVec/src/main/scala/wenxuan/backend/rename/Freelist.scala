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

package wenxuan.backend.rename

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility.{CircularQueuePtr, CircularShift, HasCircularQueuePtrHelper}
import utils.{HasPerfEvents, XSDebug, XSError, XSPerfAccumulate}
import wenxuan.common.WXModule
import wenxuan.backend.{SnapshotPort, RobCommitIO}

abstract class BaseFreeList(size: Int)(implicit p: Parameters) extends WXModule with HasCircularQueuePtrHelper {
  val io = IO(new Bundle {
    val redirect = Input(Bool())
    val walk = Input(Bool())

    val allocateReq = Input(Vec(RenameWidth, Bool()))
    val walkReq = Input(Vec(CommitWidth, Bool()))
    val allocatePhyReg = Output(Vec(RenameWidth, UInt(PhyRegIdxWidth.W)))
    val canAllocate = Output(Bool())
    val doAllocate = Input(Bool())

    val freeReq = Input(Vec(CommitWidth, Bool()))
    val freePhyReg = Input(Vec(CommitWidth, UInt(PhyRegIdxWidth.W)))

    val commit = Input(new RobCommitIO)

    val snpt = Input(new SnapshotPort)

    val debug_rat = Vec(32, Input(UInt(PhyRegIdxWidth.W)))
  })

  class FreeListPtr extends CircularQueuePtr[FreeListPtr](size)

  object FreeListPtr {
    def apply(f: Boolean, v: Int): FreeListPtr = {
      val ptr = Wire(new FreeListPtr)
      ptr.flag := f.B
      ptr.value := v.U
      ptr
    }
  }

  val lastCycleRedirect = RegNext(io.redirect, false.B)
  val lastCycleSnpt = RegNext(io.snpt, 0.U.asTypeOf(io.snpt))

  val headPtr = RegInit(FreeListPtr(false, 0))
  val headPtrOH = RegInit(1.U(size.W))
  val archHeadPtr = RegInit(FreeListPtr(false, 0))
  XSError(headPtr.toOH =/= headPtrOH, p"wrong one-hot reg between $headPtr and $headPtrOH")
  val headPtrOHShift = CircularShift(headPtrOH)
  // may shift [0, RenameWidth] steps
  val headPtrOHVec = VecInit.tabulate(RenameWidth + 1)(headPtrOHShift.left)

  val snapshots = SnapshotGenerator(headPtr, io.snpt.snptEnq, io.snpt.snptDeq, io.redirect)

  val redirectedHeadPtr = Mux(
    lastCycleSnpt.useSnpt,
    snapshots(lastCycleSnpt.snptSelect) + PopCount(io.walkReq),
    archHeadPtr + PopCount(io.walkReq)
  )
  val redirectedHeadPtrOH = Mux(
    lastCycleSnpt.useSnpt,
    (snapshots(lastCycleSnpt.snptSelect) + PopCount(io.walkReq)).toOH,
    (archHeadPtr + PopCount(io.walkReq)).toOH
  )
}

class StdFreeList(size: Int)(implicit p: Parameters) extends BaseFreeList(size) with HasPerfEvents {

  val freeList = RegInit(VecInit(Seq.tabulate(size)( i => (i + 32).U(PhyRegIdxWidth.W) )))
  val lastTailPtr = RegInit(FreeListPtr(true, 0)) // tailPtr in the last cycle (need to add freeReqReg)
  val tailPtr = Wire(new FreeListPtr) // this is the real tailPtr
  val tailPtrOHReg = RegInit(0.U(size.W))

  //
  // free committed instructions' `old_pdest` reg
  //
  val freeReqReg = io.freeReq
  for (i <- 0 until CommitWidth) {
    val offset = if (i == 0) 0.U else PopCount(freeReqReg.take(i))
    val enqPtr = lastTailPtr + offset

    // Why RegNext (from RAT and Rename): for better timing
    // Why we can RegNext: these free registers won't be used in the next cycle,
    // since we set canAllocate only when the current free regs > RenameWidth.
    when (freeReqReg(i)) {
      freeList(enqPtr.value) := io.freePhyReg(i)
    }
    XSDebug(io.freeReq(i), p"req#$i free physical reg: ${io.freePhyReg(i)}\n")
  }

  tailPtr := lastTailPtr + PopCount(freeReqReg)
  lastTailPtr := tailPtr

  //
  // allocate new physical registers for instructions at rename stage
  //
  val freeRegCnt = Wire(UInt()) // number of free registers in free list
  io.canAllocate := RegNext(freeRegCnt >= RenameWidth.U) // use RegNext for better timing
  XSDebug(p"freeRegCnt: $freeRegCnt\n")

  val phyRegCandidates = VecInit(headPtrOHVec.map(sel => Mux1H(sel, freeList)))

  for(i <- 0 until RenameWidth) {
    io.allocatePhyReg(i) := phyRegCandidates(PopCount(io.allocateReq.take(i)))
    XSDebug(p"req:${io.allocateReq(i)} canAllocate:${io.canAllocate} pdest:${io.allocatePhyReg(i)}\n")
  }
  val doCommit = io.commit.isCommit
  val archAlloc = io.commit.commitValid zip io.commit.info map { case (valid, info) => valid && info.fpWen }
  val numArchAllocate = PopCount(archAlloc)
  val archHeadPtrNew  = archHeadPtr + numArchAllocate
  val archHeadPtrNext = Mux(doCommit, archHeadPtrNew, archHeadPtr)
  archHeadPtr := archHeadPtrNext

  val isWalkAlloc = io.walk && io.doAllocate
  val isNormalAlloc = io.canAllocate && io.doAllocate
  val isAllocate = isWalkAlloc || isNormalAlloc
  val numAllocate = Mux(io.walk, PopCount(io.walkReq), PopCount(io.allocateReq))
  val headPtrAllocate = Mux(lastCycleRedirect, redirectedHeadPtr, headPtr + numAllocate)
  val headPtrOHAllocate = Mux(lastCycleRedirect, redirectedHeadPtrOH, headPtrOHVec(numAllocate))
  val headPtrNext = Mux(isAllocate, headPtrAllocate, headPtr)
  freeRegCnt := Mux(isWalkAlloc && !lastCycleRedirect, distanceBetween(tailPtr, headPtr) - PopCount(io.walkReq),
                Mux(isNormalAlloc, distanceBetween(tailPtr, headPtr) - PopCount(io.allocateReq),
                distanceBetween(tailPtr, headPtr)))

  // priority: (1) exception and flushPipe; (2) walking; (3) mis-prediction; (4) normal dequeue
  val realDoAllocate = !io.redirect && isAllocate
  headPtr := Mux(realDoAllocate, headPtrAllocate, headPtr)
  headPtrOH := Mux(realDoAllocate, headPtrOHAllocate, headPtrOH)

  XSDebug(p"head:$headPtr tail:$tailPtr\n")

  XSError(!isFull(tailPtr, archHeadPtr), "fpArchFreeList should always be full\n")

  val enableFreeListCheck = false
  if (enableFreeListCheck) {
    for (i <- 0 until size) {
      for (j <- i+1 until size) {
        XSError(freeList(i) === freeList(j), s"Found same entry in free list! (i=$i j=$j)\n")
      }
    }
  }

  XSPerfAccumulate("utilization", freeRegCnt)
  XSPerfAccumulate("allocation_blocked", !io.canAllocate)
  XSPerfAccumulate("can_alloc_wrong", !io.canAllocate && freeRegCnt >= RenameWidth.U)

  val freeRegCntReg = RegNext(freeRegCnt)
  val perfEvents = Seq(
    ("std_freelist_1_4_valid", freeRegCntReg <  (size / 4).U                                    ),
    ("std_freelist_2_4_valid", freeRegCntReg >= (size / 4).U && freeRegCntReg < (size / 2).U    ),
    ("std_freelist_3_4_valid", freeRegCntReg >= (size / 2).U && freeRegCntReg < (size * 3 / 4).U),
    ("std_freelist_4_4_valid", freeRegCntReg >= (size * 3 / 4).U                                )
  )
  generatePerfEvent()
}


class MEFreeList(size: Int)(implicit p: Parameters) extends BaseFreeList(size) with HasPerfEvents {
  val freeList = RegInit(VecInit(
    // originally {1, 2, ..., size - 1} are free. Register 0-31 are mapped to x0.
    Seq.tabulate(size - 1)(i => (i + 1).U(PhyRegIdxWidth.W)) :+ 0.U(PhyRegIdxWidth.W)))

  val tailPtr = RegInit(FreeListPtr(false, size - 1))

  val doWalkRename = io.walk && io.doAllocate && !io.redirect
  val doNormalRename = io.canAllocate && io.doAllocate && !io.redirect
  val doRename = doWalkRename || doNormalRename
  val doCommit = io.commit.isCommit

  /**
   * Allocation: from freelist (same as StdFreelist)
   */
  val phyRegCandidates = VecInit(headPtrOHVec.map(sel => Mux1H(sel, freeList)))
  for (i <- 0 until RenameWidth) {
    // enqueue instr, is move elimination
    io.allocatePhyReg(i) := phyRegCandidates(PopCount(io.allocateReq.take(i)))
  }
  // update arch head pointer
  val archAlloc = io.commit.commitValid zip io.commit.info map {
    case (valid, info) => valid && info.rfWen && !info.isMove && info.ldest =/= 0.U
  }
  val numArchAllocate = PopCount(archAlloc)
  val archHeadPtrNew  = archHeadPtr + numArchAllocate
  val archHeadPtrNext = Mux(doCommit, archHeadPtrNew, archHeadPtr)
  archHeadPtr := archHeadPtrNext

  // update head pointer
  val numAllocate = Mux(io.walk, PopCount(io.walkReq), PopCount(io.allocateReq))
  val headPtrNew   = Mux(lastCycleRedirect, redirectedHeadPtr, headPtr + numAllocate)
  val headPtrOHNew = Mux(lastCycleRedirect, redirectedHeadPtrOH, headPtrOHVec(numAllocate))
  val headPtrNext   = Mux(doRename, headPtrNew, headPtr)
  val headPtrOHNext = Mux(doRename, headPtrOHNew, headPtrOH)
  headPtr   := headPtrNext
  headPtrOH := headPtrOHNext

  /**
   * Deallocation: when refCounter becomes zero, the register can be released to freelist
   */
  for (i <- 0 until CommitWidth) {
    when (io.freeReq(i)) {
      val freePtr = tailPtr + PopCount(io.freeReq.take(i))
      freeList(freePtr.value) := io.freePhyReg(i)
    }
  }

  // update tail pointer
  val tailPtrNext = tailPtr + PopCount(io.freeReq)
  tailPtr := tailPtrNext

  val freeRegCnt = Mux(doWalkRename && !lastCycleRedirect, distanceBetween(tailPtrNext, headPtr) - PopCount(io.walkReq),
    Mux(doNormalRename,                     distanceBetween(tailPtrNext, headPtr) - PopCount(io.allocateReq),
      distanceBetween(tailPtrNext, headPtr)))
  val freeRegCntReg = RegNext(freeRegCnt)
  io.canAllocate := freeRegCntReg >= RenameWidth.U

  val debugArchHeadPtr = RegNext(RegNext(archHeadPtr, FreeListPtr(false, 0)), FreeListPtr(false, 0)) // two-cycle delay from refCounter
  val debugArchRAT = RegNext(RegNext(io.debug_rat, VecInit(Seq.fill(32)(0.U(PhyRegIdxWidth.W)))), VecInit(Seq.fill(32)(0.U(PhyRegIdxWidth.W))))
  val debugUniqPR = Seq.tabulate(32)(i => i match {
    case 0 => true.B
    case _ => !debugArchRAT.take(i).map(_ === debugArchRAT(i)).reduce(_ || _)
  })
  XSError(distanceBetween(tailPtr, debugArchHeadPtr) +& PopCount(debugUniqPR) =/= NRPhyRegs.U, "Integer physical register should be in either arch RAT or arch free list\n")

  val perfEvents = Seq(
    ("me_freelist_1_4_valid", freeRegCntReg <  (size / 4).U                                     ),
    ("me_freelist_2_4_valid", freeRegCntReg >= (size / 4).U && freeRegCntReg <= (size / 2).U    ),
    ("me_freelist_3_4_valid", freeRegCntReg >= (size / 2).U && freeRegCntReg <= (size * 3 / 4).U),
    ("me_freelist_4_4_valid", freeRegCntReg >= (size * 3 / 4).U                                 ),
  )
  generatePerfEvent()
}