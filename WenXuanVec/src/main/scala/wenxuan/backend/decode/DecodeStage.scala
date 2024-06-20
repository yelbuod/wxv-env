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
import utils.{HasPerfEvents, XSPerfAccumulate, XSPerfHistogram}
import wenxuan.common.{InstPacket, WXModule}
import wenxuan.backend.{InstCtrlPacket, CustomCSRCtrlIO}
import wenxuan.backend.rename.RatReadPort

class DecodeStage(implicit p: Parameters) extends WXModule with HasPerfEvents{
  val io = IO(new Bundle {
    val in = Vec(DecodeWidth, Flipped(DecoupledIO(new InstPacket)))
    val out = Vec(DecodeWidth, DecoupledIO(new InstCtrlPacket))
    // RAT read
    val intRatRead = Vec(RenameWidth, Vec(3, Flipped(new RatReadPort))) // data input port not used will shrink in Verilog gen
    val fpRatRead = Vec(RenameWidth, Vec(4, Flipped(new RatReadPort)))
    // csr control
    val csrCtrl = Input(new CustomCSRCtrlIO)
    // perf only
    val fusion = Vec(DecodeWidth - 1, Input(Bool())) // from fusion decoder
    val stallReason = new Bundle {
      val in = Flipped(new StallReasonIO(DecodeWidth))
      val out = new StallReasonIO(DecodeWidth)
    }
  })
  val debug_globalCounter = RegInit(0.U(XLEN.W))
  val decoders = Seq.fill(DecodeWidth)(Module(new DecodeUnit))

  for (i <- 0 until DecodeWidth) {
    decoders(i).io.enq.inst_packet := io.in(i).bits

    // csr control
    decoders(i).io.csrCtrl := io.csrCtrl

    io.out(i).bits := DontCare
    io.out(i).valid := io.in(i).valid
    io.out(i).bits := decoders(i).io.deq.inst_ctrl_packet
    // Pop count decode output fire number in Decode Width
    io.out(i).bits.ctrl.debug_globalID := debug_globalCounter + PopCount((0 until i + 1).map(io.out(_).fire))
    io.in(i).ready := io.out(i).ready

    // We use the lsrc/ldest before fusion decoder to read RAT for better timing.
    io.intRatRead(i)(0).addr := decoders(i).io.deq.inst_ctrl_packet.ctrl.lsrc(0)
    io.intRatRead(i)(1).addr := decoders(i).io.deq.inst_ctrl_packet.ctrl.lsrc(1)
    io.intRatRead(i)(2).addr := decoders(i).io.deq.inst_ctrl_packet.ctrl.ldest
    io.intRatRead(i).foreach(_.hold := !io.out(i).ready)

    // Floating-point instructions can not be fused now.
    io.fpRatRead(i)(0).addr := decoders(i).io.deq.inst_ctrl_packet.ctrl.lsrc(0)
    io.fpRatRead(i)(1).addr := decoders(i).io.deq.inst_ctrl_packet.ctrl.lsrc(1)
    io.fpRatRead(i)(2).addr := decoders(i).io.deq.inst_ctrl_packet.ctrl.lsrc(2)
    io.fpRatRead(i)(3).addr := decoders(i).io.deq.inst_ctrl_packet.ctrl.ldest
    io.fpRatRead(i).foreach(_.hold := !io.out(i).ready)
  }
  // update global counter
  debug_globalCounter := debug_globalCounter + PopCount(io.out.map(_.fire))

  val hasValid = VecInit(io.in.map(_.valid)).asUInt.orR
  io.stallReason.in.backReason := io.stallReason.out.backReason
  io.stallReason.out.reason.zip(io.stallReason.in.reason).zip(io.in.map(_.valid)).foreach { case ((out, in), valid) =>
    out := Mux(io.stallReason.out.backReason.valid,
      io.stallReason.out.backReason.bits,
      Mux(valid, TopDownCounters.NoStall.id.U, in))
  }

  XSPerfAccumulate("utilization", PopCount(io.in.map(_.valid)))
  XSPerfAccumulate("waitInstr", PopCount((0 until DecodeWidth).map(i => io.in(i).valid && !io.in(i).ready)))
  XSPerfAccumulate("stall_cycle", hasValid && !io.out(0).ready)

  XSPerfHistogram("slots_fire", PopCount(io.out.map(_.fire)), true.B, 0, DecodeWidth + 1, 1)
  XSPerfHistogram("slots_valid_pure", PopCount(io.in.map(_.valid)), io.out(0).fire, 0, DecodeWidth + 1, 1)
  XSPerfHistogram("slots_valid_rough", PopCount(io.in.map(_.valid)), true.B, 0, DecodeWidth + 1, 1)

  val fusionValid = RegNext(io.fusion)
  val inFire = io.in.map(in => RegNext(in.valid && !in.ready))
  val perfEvents = Seq(
    ("decoder_fused_instr", PopCount(fusionValid)),
    ("decoder_waitInstr", PopCount(inFire)),
    ("decoder_stall_cycle", hasValid && !io.out(0).ready),
    ("decoder_utilization", PopCount(io.in.map(_.valid))),
  )
  generatePerfEvent()
}
