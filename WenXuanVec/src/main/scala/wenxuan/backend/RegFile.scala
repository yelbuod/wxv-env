package wenxuan.backend

import chisel3._
import chisel3.util.experimental._
import difftest._
//
//class RegFile extends Module {
//  val io = IO(new Bundle {
//    val rs1_addr = Input(UInt(5.W))
//    val rs2_addr = Input(UInt(5.W))
//    val rs1_data = Output(UInt(64.W))
//    val rs2_data = Output(UInt(64.W))
//    val rd_addr = Input(UInt(5.W))
//    val rd_data = Input(UInt(64.W))
//    val rd_en = Input(Bool())
//  })
//
//  val rf = RegInit(VecInit(Seq.fill(32)(0.U(64.W))))
//
//  when (io.rd_en && (io.rd_addr =/= 0.U)) {
//    rf(io.rd_addr) := io.rd_data;
//  }
//
//  io.rs1_data := Mux((io.rs1_addr =/= 0.U), rf(io.rs1_addr), 0.U)
//  io.rs2_data := Mux((io.rs2_addr =/= 0.U), rf(io.rs2_addr), 0.U)
//
//  val dt_ar = Module(new DifftestArchIntRegState)
//  dt_ar.io.clock  := clock
//  dt_ar.io.coreid := 0.U
//  dt_ar.io.gpr    := rf
//
//  BoringUtils.addSource(rf(10), "rf_a0")
//}
