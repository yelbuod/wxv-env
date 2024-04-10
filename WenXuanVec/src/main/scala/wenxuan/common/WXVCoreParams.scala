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
import org.chipsalliance.cde.config.{Field, Parameters}
import freechips.rocketchip.tile.XLen
import wenxuan.cache.mmu.MMUParams

case class WXVCoreParams(
	fetchWidth: Int = 4,
	ftqSize: Int = 64,
  decodeWidth: Int = 2,
  numRobEntries: Int = 128,
	mmuParams: MMUParams = MMUParams(),
){
	def VAddrBits: Int = 39
}

trait HasWXCommonParameters extends HasTileParameters{

	implicit val p: Parameters

	val coreParams = tileParams.core

	val AsidLen = coreParams.mmuParams.MMUAsidLen
	val EnbaleTlbDebug = coreParams.mmuParams.EnbaleTlbDebug

	val FtqSize = coreParams.ftqSize
}
