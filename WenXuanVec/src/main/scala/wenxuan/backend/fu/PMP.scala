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

package wenxuan.backend.fu

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import wenxuan.common._
import wenxuan.cache.mmu.TlbCmd

trait PMPConst extends HasPMParameters
{
  val PMPOffBits = 2
}

abstract class PMPBundle(implicit val p: Parameters) extends Bundle with PMPConst

class PMPReqBundle(lgMaxSize: Int = 3)(implicit p: Parameters) extends PMPBundle
{
  val paddr = UInt(PMPaddrBits.W)
  val size = UInt(log2Ceil(lgMaxSize+1).W)
  val cmd = TlbCmd()

  def apply(paddr: UInt, size: UInt, cmd: UInt) {
    this.paddr := paddr
    this.size := size
    this.cmd := cmd
  }

  def apply(paddr: UInt) { // req minimal permission and req align size
    apply(paddr, lgMaxSize.U, TlbCmd.read)
  }
}

class PMPRespBundle()(implicit p: Parameters) extends PMPBundle
{
  val instr = Bool()
  val mmio = Bool()
}