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
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import org.chipsalliance.cde.config.Parameters
import utility.PipelineNext
import wenxuan.backend.decode.{DecodeStage, FusionDecoder}
import wenxuan.backend.rename._
import wenxuan.common.{FrontendToCtrlIO, HasWXCommonParameters}
import wenxuan.backendInfoType.ExceptionNO.{instrAccessFault, instrPageFault}
import wenxuan.backend.CustomCSRCtrlIO
import xiangshan.Redirect

class CtrlBlock (implicit p: Parameters) extends LazyModule {
  lazy val module = new CtrlBlockImp(this)

}

class CtrlBlockImp(outer: CtrlBlock)(implicit p: Parameters) extends LazyModuleImp(outer)
  with HasWXCommonParameters {
  val io = IO(new Bundle {
    val hartId = Input(UInt(8.W))
    val cpu_halt = Output(Bool())
    val frontend = Flipped(new FrontendToCtrlIO)
    val csrCtrl = Input(new CustomCSRCtrlIO)
  })

  val stage2Redirect = Wire(Valid(new Redirect))

  /** Frontend instr packet -> Decode */
  val decode = Module(new DecodeStage)
  val fusionDecoder = Module(new FusionDecoder)
  decode.io.in <> io.frontend.instPackVec
  decode.io.csrCtrl := RegNext(io.csrCtrl)
  decode.io.stallReason.in <> io.frontend.stallReason

  /** Rename Map Table(Register Alias Table) */
  val rat = Module(new RenameTableWrapper) // int & fp rat wrapper
  rat.io.intReadPorts <> decode.io.intRatRead
  rat.io.fpReadPorts <> decode.io.fpRatRead

  /** Pipeline between decode and rename */
  val rename = Module(new Rename)
  for(i <- 0 until RenameWidth) {
    val decodeHasException = io.frontend.instPackVec(i).bits.exceptionVec(instrPageFault) ||
      io.frontend.instPackVec(i).bits.exceptionVec(instrAccessFault)
    val disableFusion = decode.io.csrCtrl.singlestep || !decode.io.csrCtrl.fusion_enable
    fusionDecoder.io.in(i).valid := io.frontend.instPackVec(i).valid && !(disableFusion || decodeHasException)
    fusionDecoder.io.in(i).bits := io.frontend.instPackVec(i).bits.instr
    if(i > 0) {
      fusionDecoder.io.inReady(i - 1) := decode.io.out(i).ready
    }

    // Pipeline
    val renamePipe = PipelineNext(decode.io.out(i), rename.io.in(i).ready,
      stage2Redirect.valid || pendingRedirect)
    renamePipe.ready := rename.io.in(i).ready // rename input ready inform to decode pipe
    rename.io.in(i).valid := renamePipe.valid && !fusionDecoder.io.clear(i)
    rename.io.in(i).bits := renamePipe.bits
    // lsrc -> psrc mapping read from rat to rename module
    rename.io.intReadPorts(i) := rat.io.intReadPorts(i).map(_.data)
    rename.io.fpReadPorts(i) := rat.io.fpReadPorts(i).map(_.data)
    rename.io.waittable(i) := RegEnable(waittable.io.rdata(i), decode.io.out(i).fire)
    rename.io.ssit(i) := ssit.io.rdata(i)
    // new allocate pdest mapping write into rat
    rat.io.intRenamePorts(i) := rename.io.intRenamePorts(i)
    rat.io.fpRenamePorts(i) := rename.io.fpRenamePorts(i)
  }

  /**
   * Snapshot: rename output snapshot signal to inform all arch resources to store snapshot
   *  1. robIdx
   *  2. rename alias table(RAT)
   *  3. freelistPtr of freelist inside rename module
   *  4. robPtr (snapshot after rename -> dispatch)
   */
  val snpt = Module(new SnapshotGenerator(rename.io.out.head.bits.robIdx))
  snpt.io.enq := rename.io.out.head.bits.snapshot && rename.io.out.head.fire
  snpt.io.enqData.head := rename.io.out.head.bits.robIdx
  snpt.io.deq := snpt.io.valids(snpt.io.deqPtr.value) && rob.io.commits.isCommit &&
    Cat(rob.io.commits.commitValid.zip(rob.io.commits.robIdx).map(x => x._1 && x._2 === snpt.io.snapshots(snpt.io.deqPtr.value))).orR
  snpt.io.flush := stage2Redirect.valid

  val useSnpt = VecInit.tabulate(RenameSnapshotNum)(idx =>
    snpt.io.valids(idx) && stage2Redirect.bits.robIdx >= snpt.io.snapshots(idx)).reduceTree(_ || _)
  val snptSelect = MuxCase(0.U(log2Ceil(RenameSnapshotNum).W),
    (1 to RenameSnapshotNum).map(i => (snpt.io.enqPtr - i.U).value).map(idx =>
      (snpt.io.valids(idx) && stage2Redirect.bits.robIdx >= snpt.io.snapshots(idx), idx)
    ))

  rat.io.snpt.snptEnq := rename.io.out.head.bits.snapshot && rename.io.out.head.fire
  rat.io.snpt.snptDeq := snpt.io.deq
  rat.io.snpt.useSnpt := useSnpt
  rat.io.snpt.snptSelect := snptSelect

  rename.io.snpt.snptEnq := DontCare
  rename.io.snpt.snptDeq := snpt.io.deq
  rename.io.snpt.useSnpt := useSnpt
  rename.io.snpt.snptSelect := snptSelect

  rob.io.snpt.snptEnq := DontCare
  rob.io.snpt.snptDeq := snpt.io.deq
  rob.io.snpt.useSnpt := useSnpt
  rob.io.snpt.snptSelect := snptSelect

  /**
   * ROB commit release old allocated pdest or WALK resume allocate after redirect
   */
  rat.io.redirect := stage2Redirect.valid
  rat.io.robCommits := rob.io.commits
  rename.io.redirect := stage2Redirect
  rename.io.robCommits := rob.io.commits
  rename.io.int_need_free := rat.io.int_need_free
  rename.io.int_old_pdest := rat.io.int_old_pdest
  rename.io.fp_old_pdest := rat.io.fp_old_pdest
  rename.io.debug_int_rat <> rat.io.debug_int_rat
  rename.io.debug_fp_rat <> rat.io.debug_fp_rat
  rename.io.stallReason.in <> decode.io.stallReason.out

}