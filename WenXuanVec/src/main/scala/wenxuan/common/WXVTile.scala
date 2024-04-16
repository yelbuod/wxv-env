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

import chisel3._
import chisel3.util._
import coupledL2.L2Param
import coupledL2.prefetch._
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
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
  dcacheOpt: Option[DCacheParams] = Some(DCacheParams()),
  l2cacheOpt: Option[L2Param] = Some(L2Param(
    ways = 8,
    sets = 512, // default 256KB L2 : 64B * 8(way) * 512 = 64*4*2*512 = 64*4*1024B = 256KB
    prefetch = Some(coupledL2.prefetch.PrefetchReceiverParams())
  )),
  name: Option[String] = Some("wenxuanvec_tile"),
  hartId: Int = 0
)

trait HasTileParameters {
  implicit val p: Parameters

  // why use 'def' instead of 'val', cause 'val' will exactly create a whole WXVTileParams type instance
  //  in which extends HasTileParameters, even if you don't need all members of HasTileParameters
  def tileParams: WXVTileParams = p(WXVTileKey)
  // to satisfy the nextline icache fetch arch design
  require(tileParams.core.fetchWidth * 4 == tileParams.icache.blockBytes / 2)
  
  val XLEN: Int = p(XLen)

  val HartId = tileParams.hartId

  val PAddrBits = p(SoCParamsKey).PAddrBits // PAddrBits is Phyical Memory addr bits
  //  val coreParams: WXVCoreParams = tileParams.core
  val VAddrBits: Int = tileParams.core.VAddrBits

  // cache hierarchy configurations
  val l1BusDataWidth_Bits = 256

}

class WXVTile()(implicit p: Parameters) extends LazyModule
  with HasTileParameters
{
  override def shouldBeInlined: Boolean = false
  // outer submodule
  val core = LazyModule(new WXVCore())
  val l2top   = LazyModule(new L2Top())

  // outer submodule interconnect
  l2top.misc_l2_pmu := l2top.l1i_logger := core.frontend.icache.clientNode
  l2top.l1_xbar :=* l2top.misc_l2_pmu

  val l2cache = l2top.l2cache
  l2cache match {
    case Some(l2) =>
      l2.node :*= l2top.xbar_l2_buffer :*= l2top.l1_xbar
    case None =>
  }


  if (l2cache.isDefined) {
    // TODO: add ECC interface of L2

//    l2top.module.beu_errors.l2 <> 0.U.asTypeOf(l2top.module.beu_errors.l2)
//    core.module.io.l2_hint.bits.sourceId := l2top.module.l2_hint.bits.sourceId
//    core.module.io.l2_hint.bits.isKeyword := l2top.module.l2_hint.bits.isKeyword
//    core.module.io.l2_hint.valid := l2top.module.l2_hint.valid
//
//    core.module.io.l2PfqBusy := false.B
//    core.module.io.debugTopDown.l2MissMatch := l2top.module.debugTopDown.l2MissMatch
//    l2top.module.debugTopDown.robHeadPaddr := core.module.io.debugTopDown.robHeadPaddr
  } else {

//    l2top.module.beu_errors.l2 <> 0.U.asTypeOf(l2top.module.beu_errors.l2)
//    core.module.io.l2_hint.bits.sourceId := l2top.module.l2_hint.bits.sourceId
//    core.module.io.l2_hint.bits.isKeyword := l2top.module.l2_hint.bits.isKeyword
//    core.module.io.l2_hint.valid := l2top.module.l2_hint.valid
//
//    core.module.io.l2PfqBusy := false.B
//    core.module.io.debugTopDown.l2MissMatch := false.B
  }

  // tile module implementation: submodule interconnect (core and l2) and connect with outer node module (core, l2)
  lazy val module = new WXVTileImp(this)
}

class WXVTileImp(outer: WXVTile) extends LazyModuleImp(outer)
  with HasTileParameters
{

}