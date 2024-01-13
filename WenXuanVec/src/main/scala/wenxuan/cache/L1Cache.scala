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

package wenxuan.cache

import wenxuan.common.HasTileParameters

trait L1CacheParams {
  def nSets:         Int
  def nWays:         Int
  def rowBits:       Int
  def nTLBSets:      Int
  def nTLBWays:      Int
  def blockBytes:    Int // TODO this is ignored in favor of p(CacheBlockBytes) in BaseTile
}

trait HasL1CacheParameters extends HasTileParameters{
  val cacheParams: L1CacheParams

}
