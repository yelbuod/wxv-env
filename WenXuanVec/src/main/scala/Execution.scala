import chisel3._
import chisel3.util._

class Execution extends Module {
  val io = IO(new Bundle {
    val opcode = Input(UInt(8.W))
    val in1 = Input(UInt(64.W))
    val in2 = Input(UInt(64.W))
    val pc  = Input(UInt(64.W))
    val addr_gen = Input(Bool())
    val out = Output(UInt(64.W))
    val dmem = new RamIO
    val pc_jaddr = Valid(UInt(64.W))
  })

  io.out := 0.U

  when (io.opcode === 1.U) {
    when (io.addr_gen === true.B) {
      io.out := io.pc + 4.U
      // printf("exe io out = %x\n", io.out)
    }.otherwise {
      // printf("io.in1=%x, io.in2=%x, exe io out=%x\n", io.in1, io.in2, io.out)
      io.out := io.in1 + io.in2
    }
  }

  when (io.addr_gen === true.B) {
    io.pc_jaddr.valid := true.B
    io.pc_jaddr.bits  := io.in1 + io.in2
  }.otherwise {
    io.pc_jaddr.valid := false.B
    io.pc_jaddr.bits  := 0.U
  }

  io.dmem.en := false.B
  io.dmem.addr := 0.U
  io.dmem.wen := false.B
  io.dmem.wdata := 0.U
  io.dmem.wmask := 0.U

}
