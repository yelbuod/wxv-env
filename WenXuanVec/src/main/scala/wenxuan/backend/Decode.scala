package wenxuan.backend

import chisel3._
import chisel3.util._
import Instructions._

class Decode extends Module {
  val io = IO(new Bundle {
    val inst = Input(UInt(32.W))
    val rs1_addr = Output(UInt(5.W))
    val rs1_en = Output(UInt(2.W))
    val rs2_addr = Output(UInt(5.W))
    val rs2_en = Output(UInt(2.W))
    val rd_addr = Output(UInt(5.W))
    val rd_en = Output(Bool())
    val opcode = Output(UInt(8.W))
    val imm = Output(UInt(64.W))
    val addr_gen = Output(Bool())
  })

  val inst = io.inst
  val opcode = WireInit(UInt(8.W), 0.U)
  val addr_gen = WireInit(Bool(), false.B)
  val imm_i = Cat(Fill(64-12, inst(31)), inst(31, 20))
  val imm_u = Cat(Fill(64-32, inst(31)), Cat(inst(31, 12), 0.U(12.W)))
  val imm_j = Cat(Fill(64-21, inst(31)), inst(31), inst(19, 12), inst(20), inst(30, 25), inst(24, 21), 0.U(1.W))

  // Only example here, use your own control flow!
  when (inst === ADDI) {
    opcode := 1.U
    io.imm := imm_i
  }.elsewhen(inst === AUIPC) {
    opcode := 1.U
    io.imm := imm_u
  }.elsewhen(inst === LUI) {
    opcode := 1.U
    io.imm := imm_u
  }.elsewhen(inst === JAL) {
    opcode := 1.U
    io.imm := imm_j
    addr_gen := true.B
  }.elsewhen(inst === JALR) {
    opcode := 1.U
    io.imm := imm_i
    addr_gen := true.B
  }.otherwise {
    io.imm := 0.U(64.W)
  }

  io.rs1_addr := inst(19, 15)
  io.rs2_addr := inst(24, 20)
  io.rd_addr := inst(11, 7)
  
  io.rs1_en := 0.U(2.W)
  io.rs2_en := 0.U(2.W)
  io.rd_en := false.B

  when (inst === ADDI) {
    io.rs1_en := 1.U(2.W)
    io.rs2_en := 2.U(2.W)
    io.rd_en := true.B
  }.elsewhen (inst === AUIPC) {
    io.rs1_en := 2.U(2.W)
    io.rs2_en := 2.U(2.W)
    io.rd_en := true.B
  }.elsewhen (inst === LUI) {
    io.rs1_en := 0.U(2.W)
    io.rs2_en := 2.U(2.W)
    io.rd_en := true.B
  }.elsewhen (inst === JAL) {
    io.rs1_en := 2.U(2.W)
    io.rs2_en := 2.U(2.W)
    io.rd_en := true.B
  }.elsewhen (inst === JALR) {
    io.rs1_en := 1.U(2.W)
    io.rs2_en := 2.U(2.W)
    io.rd_en := true.B
  }
  
  io.opcode := opcode
  io.addr_gen := addr_gen
}
