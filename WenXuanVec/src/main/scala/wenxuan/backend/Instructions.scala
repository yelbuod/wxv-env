package wenxuan.backend

import chisel3._
import chisel3.util._

object Instructions {
  def ADDI    = BitPat("b?????????????????000?????0010011")
  def AUIPC   = BitPat("b?????????????????????????0010111")
  def LUI     = BitPat("b?????????????????????????0110111")
  def JAL     = BitPat("b?????????????????????????1101111")
  def JALR    = BitPat("b?????????????????000?????1100111")
}
