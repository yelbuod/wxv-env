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

import coupledL2.L2Param
import freechips.rocketchip.tile.XLen
import org.chipsalliance.cde.config.{Config, Field, Parameters}
import system.SoCParamsKey
import wenxuan.frontend.icache.ICacheParams
import wenxuan.cache.DCacheParams

case object WXVTileKey extends Field[WXVTileParams]

// Tile: Core + L2 Cache
case class WXVTileParams(
  core: WXVCoreParams = WXVCoreParams(),
  icache: ICacheParams = ICacheParams(),
  dcache: Option[DCacheParams] = Some(DCacheParams()),
  l2cache: Option[L2Param] = None,
  name: Option[String] = Some("wenxuanvec_tile"),
  hartId: Int = 0
)



class WXVTile()(implicit p: Parameters) {

}

trait HasTileParameters {
  implicit val p: Parameters

  // why use 'def' instead of 'val', cause 'val' will exactly create a whole WXVTileParams type instance
  //  in which extends HasTileParameters, even if you don't need all members of HasTileParameters
  def tileParams: WXVTileParams = p(WXVTileKey)

  val XLEN: Int = p(XLen)

  val PAddrBits = p(SoCParamsKey).PAddrBits // PAddrBits is Phyical Memory addr bits
  //  val coreParams: WXVCoreParams = tileParams.core
  val VAddrBits: Int = tileParams.core.VAddrBits

  // cache hierarchy configurations
  val l1BusDataWidth = 256
}