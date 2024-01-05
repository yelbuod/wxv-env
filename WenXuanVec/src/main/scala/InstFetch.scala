import chisel3._
import chisel3.util._

class InstFetch extends Module {
  val io = IO(new Bundle {
    val imem = new RomIO
    val pc = Output(UInt(64.W))
    val inst = Output(UInt(32.W))
    val pc_jaddr = Flipped(Valid(UInt(64.W)))
    val pc_en = Output(Bool())
  })

  val pc_en = RegInit(false.B)
  pc_en := true.B

  val pc = RegInit("h80000000".U(64.W))
  val pc_next = Mux(io.pc_jaddr.valid, io.pc_jaddr.bits, pc + 4.U)
  pc := pc_next

  io.imem.en := true.B
  io.imem.addr := pc.asUInt()

  io.pc := pc
  io.inst := io.imem.rdata(31, 0)
  io.pc_en := pc_en
}
