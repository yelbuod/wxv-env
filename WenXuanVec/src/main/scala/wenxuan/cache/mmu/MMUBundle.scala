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

package wenxuan.cache.mmu

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import wenxuan.common._
import wenxuan.backend.rob.RobPtr

abstract class TlbBundle(implicit p: Parameters) extends WXBundle

/** backend Mem block */
class MemBlockidxBundle(implicit p: Parameters) extends TlbBundle
{

}

class TlbReq(implicit p: Parameters) extends TlbBundle {
  val vaddr = Output(UInt(VAddrBits.W))
  val cmd = Output(TlbCmd())
  val size = Output(UInt(log2Ceil(log2Ceil(XLEN/8)+1).W))
  val kill = Output(Bool()) // Use for blocked tlb that need sync with other module like icache
  val memidx = Output(new MemBlockidxBundle)
  // do not translate, but still do pmp/pma check
  val no_translate = Output(Bool())
  val debug = new Bundle {
    val pc = Output(UInt(XLEN.W))
    val robIdx = Output(new RobPtr)
    val isFirstIssue = Output(Bool())
  }

  // Maybe Block req needs a kill: for itlb, itlb and icache may not sync, itlb should wait icache to go ahead
  override def toPrintable: Printable = {
    p"vaddr:0x${Hexadecimal(vaddr)} cmd:${cmd} kill:${kill} pc:0x${Hexadecimal(debug.pc)} robIdx:${debug.robIdx}"
  }
}

class TlbExceptionBundle(implicit p: Parameters) extends TlbBundle
{
  val ld = Output(Bool())
  val st = Output(Bool())
  val instr = Output(Bool())
}

class TlbResp(nDups: Int = 1)(implicit p: Parameters) extends TlbBundle {
  val paddr = Vec(nDups, Output(UInt(PAddrBits.W)))
  val miss = Output(Bool())
  val excp = Vec(nDups, new Bundle {
    val pf = new TlbExceptionBundle()
    val af = new TlbExceptionBundle()
  })
  val ptwBack = Output(Bool()) // when ptw back, wake up replay rs's state
  val memidx = Output(new MemBlockidxBundle)

  val debug = new Bundle {
    val robIdx = Output(new RobPtr)
    val isFirstIssue = Output(Bool())
  }
  override def toPrintable: Printable = {
    p"paddr:0x${Hexadecimal(paddr(0))} miss:${miss} excp.pf: ld:${excp(0).pf.ld} st:${excp(0).pf.st} instr:${excp(0).pf.instr} ptwBack:${ptwBack}"
  }
}

class TlbRequestIO(nRespDups: Int = 1)(implicit p: Parameters) extends TlbBundle {
  val req = DecoupledIO(new TlbReq)
  val req_kill = Output(Bool())
  val resp = Flipped(DecoupledIO(new TlbResp(nRespDups)))
}

object TlbCmd {
  def read = "b00".U
  def write = "b01".U
  def exec = "b10".U

  def apply() = UInt(3.W)
}
