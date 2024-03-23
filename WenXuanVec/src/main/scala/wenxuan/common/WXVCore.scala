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
import chisel3.util.experimental._
import chisel3.util._
import difftest._
import wenxuan.backend._
import wenxuan.frontend._
import wenxuan.mem._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy.{LazyModuleImp, LazyModule}
abstract class WXModule(implicit val p: Parameters) extends Module
  with HasWXCommonParameters

abstract class WXBundle(implicit val p: Parameters) extends Bundle
  with HasWXCommonParameters

abstract class WXVCoreBase()(implicit p: Parameters) extends LazyModule
  with HasWXCommonParameters
{
  override def shouldBeInlined: Boolean = false
  // outer submodule
  val frontend = LazyModule(new Frontend())

}

class WXVCore()(implicit p: Parameters) extends WXVCoreBase
{
  // core module implementation: submodule interconnect and connect with outer node module (frontend)
  lazy val module = new WXVCoreImp(this)
}

class WXVCoreImp(outer: WXVCore) extends LazyModuleImp(outer)
  with HasWXCommonParameters
{
  val io = IO(new Bundle {
    val beu_errors = Output(new WXL1BusErrors) // icache & dcache bus error
  })

  private val frontend = outer.frontend.module

  io.beu_errors.icache := frontend.io.error.toL1BusErrorUnitInfo() // ECC error to beu

}

//
//class WXVCore extends Module {
//  val io = IO(new Bundle {
//    val imem = new RomIO
//    val dmem = new RamIO
//  })
//
//  val fetch = Module(new InstFetch)
//  fetch.io.imem <> io.imem
//
//
//  val decode = Module(new Decode)
//  decode.io.inst := fetch.io.inst
//
//  val rf = Module(new RegFile)
//  rf.io.rs1_addr := decode.io.rs1_addr
//  rf.io.rs2_addr := decode.io.rs2_addr
//  rf.io.rd_addr := decode.io.rd_addr
//  rf.io.rd_en := decode.io.rd_en
//
//  val execution = Module(new Execution)
//  execution.io.opcode := decode.io.opcode
//  execution.io.addr_gen := decode.io.addr_gen
//  execution.io.pc := fetch.io.pc
//  execution.io.pc_jaddr <> fetch.io.pc_jaddr
//  execution.io.in1 := Mux1H(Seq((decode.io.rs1_en === 1.U, rf.io.rs1_data),
//                                (decode.io.rs1_en === 2.U, fetch.io.pc),
//                                (decode.io.rs1_en === 0.U, 0.U)
//  ))
//  // execution.io.in1 := Mux(decode.io.rs1_en, rf.io.rs1_data, 0.U)
//  execution.io.in2 := Mux1H(Seq((decode.io.rs2_en === 1.U, rf.io.rs2_data),
//                                (decode.io.rs2_en === 2.U, decode.io.imm),
//                                (decode.io.rs2_en === 0.U, 4.U),
//  ))
//  // execution.io.in2 := Mux(decode.io.rs2_en, rf.io.rs2_data, decode.io.imm)
//  execution.io.dmem <> io.dmem
//  rf.io.rd_data := execution.io.out
//
//  // printf("fetch pc = %x\n", fetch.io.pc)
//  /* ----- Difftest ------------------------------ */
//
//  val dt_ic = Module(new DifftestInstrCommit)
//  dt_ic.io.clock    := clock
//  dt_ic.io.coreid   := 0.U
//  dt_ic.io.index    := 0.U
//  dt_ic.io.valid    := fetch.io.pc_en
//  dt_ic.io.pc       := RegNext(fetch.io.pc)
//  dt_ic.io.instr    := RegNext(fetch.io.inst)
//  dt_ic.io.skip     := false.B
//  dt_ic.io.isRVC    := false.B
//  dt_ic.io.scFailed := false.B
//  dt_ic.io.wen      := RegNext(decode.io.rd_en)
//  dt_ic.io.wdata    := RegNext(execution.io.out)
//  dt_ic.io.wdest    := RegNext(decode.io.rd_addr)
//
//  val dt_ae = Module(new DifftestArchEvent)
//  dt_ae.io.clock        := clock
//  dt_ae.io.coreid       := 0.U
//  dt_ae.io.intrNO       := 0.U
//  dt_ae.io.cause        := 0.U
//  dt_ae.io.exceptionPC  := 0.U
//
//  val cycle_cnt = RegInit(0.U(64.W))
//  val instr_cnt = RegInit(0.U(64.W))
//
//  cycle_cnt := cycle_cnt + 1.U
//  instr_cnt := instr_cnt + 1.U
//
//  val rf_a0 = WireInit(0.U(64.W))
//  BoringUtils.addSink(rf_a0, "rf_a0")
//
//  val dt_te = Module(new DifftestTrapEvent)
//  // 需要延迟一拍？（usage说要，但是香山里并未延迟）
//  dt_te.io.clock    := clock
//  dt_te.io.coreid   := 0.U
//  dt_te.io.valid    := (fetch.io.inst === "h0000006b".U)
//  dt_te.io.code     := rf_a0(2, 0)
//  dt_te.io.pc       := fetch.io.pc
//  dt_te.io.cycleCnt := cycle_cnt
//  dt_te.io.instrCnt := instr_cnt
//
//  val dt_cs = Module(new DifftestCSRState)
//  dt_cs.io.clock          := clock
//  dt_cs.io.coreid         := 0.U
//  dt_cs.io.priviledgeMode := 3.U  // Machine mode
//  dt_cs.io.mstatus        := 0.U
//  dt_cs.io.sstatus        := 0.U
//  dt_cs.io.mepc           := 0.U
//  dt_cs.io.sepc           := 0.U
//  dt_cs.io.mtval          := 0.U
//  dt_cs.io.stval          := 0.U
//  dt_cs.io.mtvec          := 0.U
//  dt_cs.io.stvec          := 0.U
//  dt_cs.io.mcause         := 0.U
//  dt_cs.io.scause         := 0.U
//  dt_cs.io.satp           := 0.U
//  dt_cs.io.mip            := 0.U
//  dt_cs.io.mie            := 0.U
//  dt_cs.io.mscratch       := 0.U
//  dt_cs.io.sscratch       := 0.U
//  dt_cs.io.mideleg        := 0.U
//  dt_cs.io.medeleg        := 0.U
//}
