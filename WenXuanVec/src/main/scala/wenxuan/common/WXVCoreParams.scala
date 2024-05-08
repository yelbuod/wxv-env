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
import wenxuan.backend.exu.ExuParams

case class WXVCoreParams(
  HasCExtension: Boolean = true,
	// number of inst in fetch block and consider each instr is normal instr(4-byte)
	fetchWidth: Int = 8,
	ftqSize: Int = 64,
  decodeWidth: Int = 6,
	nrPhyRegs: Int = 192,
	commitWidth: Int = 6,
  numRobEntries: Int = 128,
	mmuParams: MMUParams = MMUParams(),
	exuParams: ExuParams = ExuParams(
		JmpCnt = 1,
		AluCnt = 4,
		MulCnt = 0,
		MduCnt = 2,
		FmacCnt = 4,
		FmiscCnt = 2,
		FmiscDivSqrtCnt = 0,
		LduCnt = 2,
		StuCnt = 2
	),
){
	def VAddrBits: Int = 39
	val HistoryLength = 1 // TODO
}

trait HasWXCommonParameters extends HasTileParameters{

	implicit val p: Parameters

	val coreParams = tileParams.core

	val AsidLen = coreParams.mmuParams.MMUAsidLen
	val EnbaleTlbDebug = coreParams.mmuParams.EnbaleTlbDebug
	val HasCExtension = coreParams.HasCExtension

	val HistoryLength = coreParams.HistoryLength
	val FtqSize = coreParams.ftqSize
	/** Fetch Block */
	// the unit of fetchWidth is 32-bit(4-byte), means ICache always fetch 4*fetchWidth byte inst block
	// and 4*fetchWidth always equals to icache.blockBytes/2 to support nextline icache fetch design performance
	require(tileParams.core.fetchWidth * 4 == tileParams.icache.blockBytes / 2)
	val FetchWidth = coreParams.fetchWidth
	// number of Predict inst in fetch block and consider each instr is Compress instr(2-byte)
	val PredictWidth = FetchWidth * (if (HasCExtension) 2 else 1)
	val instBytes = if (HasCExtension) 2 else 4
	val instOffsetBits = log2Ceil(instBytes)
	val InstFetchBlockBytes = PredictWidth * instBytes
	val InstFetchBlockOffBits = log2Ceil(InstFetchBlockBytes)

	val DecodeWidth = coreParams.decodeWidth

	val exuParams = coreParams.exuParams
	val NumRedirect = exuParams.JmpCnt + exuParams.AluCnt
	val FtqRedirectAheadNum = exuParams.AluCnt
	val BackendRedirectNum = NumRedirect + 2 //2: ldReplay + Exception

	val NRPhyRegs = coreParams.nrPhyRegs
	val PhyRegIdxWidth = log2Up(NRPhyRegs)

	val CommitWidth = coreParams.commitWidth
}
