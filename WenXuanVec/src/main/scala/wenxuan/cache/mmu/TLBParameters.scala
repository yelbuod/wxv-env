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

case class TLBParams
(
  name: String = "none",
  fenceDelay: Int = 2,
  // dmode(debug mode) depends on whether the load/store privilege defined in mstatus's mpp field are take into account.
  // so TLB for load/store must use dmode to get the corresponding privilege level, while the TLB for fetch does not.
  useDmode: Boolean = true,
  ifetch: Boolean = false, // use to block instr fetch from S-Mode to U-Mode
  outReplace: Boolean = false, // mutilple tlb use the same outer replacer
  // default fully-associate TLB
  nSets: Int = 1,
  nWays: Int = 48,
  Replacer: Option[String] = Some("plru"),
  outsideRecvFlush: Boolean = false, // if outside module waiting for tlb recv flush pipe
){
  val Associative: String = "fully-associative" // must be fa
}

case class MMUParams
(
  name: String = "MMU",
  EnbaleTlbDebug: Boolean = false,
  MMUAsidLen: Int = 16, // max is 16, 0 is not supported now
  itlb: TLBParams = TLBParams(),
)