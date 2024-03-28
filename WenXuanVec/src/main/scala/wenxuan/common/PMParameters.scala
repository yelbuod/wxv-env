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

import chisel3.util._
import freechips.rocketchip.tile.XLen
import org.chipsalliance.cde.config.{Field, Parameters}
import system.SoCParamsKey

/** Physical Memory Parameters */
case object PMParamsKey extends Field[PMParams]

/** Memory Mapped PMA */
case class MMPMAConfig
(
  address: BigInt,
  mask: BigInt,
  lgMaxSize: Int,
  sameCycle: Boolean,
  num: Int
)

case class PMParams
(
  // the number of PMP and PMA entries
  NumPMP: Int = 16,
  NumPMA: Int = 16,

  // platform PMP region grain
  PlatformGrain: Int = log2Ceil(4*1024), // 4KB, a normal page

  mmpma: MMPMAConfig = MMPMAConfig(
    address = 0x38021000,
    mask = 0xfff,
    lgMaxSize = 3,
    sameCycle = true,
    num = 2
  )
)

//trait HasPMParameters extends PMParameters
trait HasPMParameters {
  implicit val p: Parameters

  val PMPAddrBits = p(SoCParamsKey).PAddrBits
  val PMXLEN = p(XLen)
  val pmParams = p(PMParamsKey)
  val NumPMP = pmParams.NumPMP
  val NumPMA = pmParams.NumPMA

  val PlatformGrain = pmParams.PlatformGrain
  val mmpma = pmParams.mmpma
}