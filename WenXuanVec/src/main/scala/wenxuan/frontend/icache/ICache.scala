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

package wenxuan.frontend.icache

import chisel3._
import chisel3.util._

import wenxuan.cache.{HasL1CacheParameters, L1CacheParams}
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}

// 32KB, 4-way, 64-blockByte
case class ICacheParams(
  nSets: Int = 128,
  nWays: Int = 4,
  rowBits: Int = 64,
  nTLBSets: Int = 1,
  nTLBWays: Int = 32,
  blockBytes: Int = 64
) extends L1CacheParams {

}

trait HasICacheParameters extends HasL1CacheParameters {
  val cacheParams = tileParams.icache
}

class ICache()(implicit p: Parameters) extends LazyModule with HasICacheParameters {
  lazy val module = new ICacheImp(this)
}

class ICacheImp(outer: ICache) extends LazyModuleImp(outer) with HasICacheParameters {

}
