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
import wenxuan.backend.fu.PMPReqBundle

class TLBIO(PortNum: Int, nRespDups: Int = 1, q: TLBParams)(implicit p: Parameters)
  extends MMUIOBaseBundle
{
  val hartId = Input(UInt(8.W))
  val requestor = Vec(PortNum, Flipped(new TlbRequestIO(nRespDups)))
  val pmp = Vec(PortNum, new PMPReqBundle)
}

/** TLB Module
 * support block and non-block request io at the same time
 * return paddr at next cycle
 * @param PortNum: the number of requestors
 * @param nRespDups: the duplicate of response
 * @param Block: Blocked or non-block attributes of each request port
 * @param q: TLB Parameters, like entry number, each TLB has its own params
 */
class TLB(PortNum: Int, nRespDups: Int = 1, Block: Seq[Boolean], q: TLBParams)(implicit p: Parameters)
  extends TlbModule
{
  val io  = IO(new TLBIO(PortNum, nRespDups, q))

  val req = io.requestor.map(_.req) // Vec(PortNum)
  val resp = io.requestor.map(_.resp) // Vec(PortNum, Vec(nRespDups))

  val csr = io.csr // privilege issues
  val mode = if (q.useDmode) csr.priv.dmode else csr.priv.imode // privilege level


}
