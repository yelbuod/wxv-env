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
import freechips.rocketchip.tilelink._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy.{IdRange, LazyModule, LazyModuleImp}
import freechips.rocketchip.util.BundleFieldBase
import xiangshan.cache.DCacheParameters
import huancun.{AliasField, PrefetchField}
import utility._
import wenxuan.common._
import wenxuan.frontend._
import wenxuan.cache._

// 32KB, 4-way, 64-blockByte
case class ICacheParams(
  nSets: Int = 128,
  nWays: Int = 4,
  rowBits: Int = 64,
  nTLBSets: Int = 1,
  nTLBWays: Int = 32,
  blockBytes: Int = 64,
  nMissEntries: Int = 2,
  enableICachePrefetch: Boolean = true, // enable FDIP Prefetch module prefetch to L2
  prefetchToL1: Boolean = false, // FDIP Prefetch module move to L1 Cache meta/data
  prefetchPipeNum: Int = 1, // prefetch request number
  nPrefetchEntries: Int = 12, // prefetch Issue Queue entry number
  nPrefBufferEntries: Int = 32, // prefetch buffer entry number
  maxIPFMoveConf: Int = 1, // confidence threshold of prefetch buffer move to L1 Cache
) extends L1CacheParams {

  val setBytes = nSets * blockBytes
  val aliasBitsOpt = DCacheParameters().aliasBitsOpt //if(setBytes > pageSize) Some(log2Ceil(setBytes / pageSize)) else None
  val reqFields: Seq[BundleFieldBase] = Seq(
    PrefetchField(),
    ReqSourceField()
  ) ++ aliasBitsOpt.map(AliasField)
  val echoFields: Seq[BundleFieldBase] = Nil
}

trait HasICacheParameters extends HasL1CacheParameters {
  val cacheParams = tileParams.icache

  def PortNumber = 2 // icache support 2 request channel

  def enableICachePrefetch = cacheParams.enableICachePrefetch
  def prefetchToL1 = cacheParams.prefetchToL1
  def prefetchPipeNum = cacheParams.prefetchPipeNum
}

abstract class ICacheBundle(implicit p: Parameters) extends WXBundle
  with HasICacheParameters

class ICachePMPBundle(implicit p: Parameters) extends ICacheBundle{
  val req  = Valid(new PMPReqBundle())
  val resp = Input(new PMPRespBundle())
}

class ICacheIO(implicit p: Parameters) extends ICacheBundle
{
  val hartId = Input(UInt(8.W))
  val prefetch    = Flipped(new FtqPrefechBundle)
  val stop        = Input(Bool())
  val fetch       = new ICacheMainPipeBundle
  val toIFU       = Output(Bool())
  val pmp         = Vec(PortNumber + prefetchPipeNum, new ICachePMPBundle)
  val itlb        = Vec(PortNumber + prefetchPipeNum, new TlbRequestIO)
  val error       = new L1CacheErrorInfo
  val fencei      = Input(Bool())
  /* Cache Instruction */
  val csr         = new L1CacheToCsrIO
  /* CSR control signal */
  val csr_pf_enable = Input(Bool())
  val csr_parity_enable = Input(Bool())
}

class ICache()(implicit p: Parameters) extends LazyModule with HasICacheParameters {
  override def shouldBeInlined: Boolean = false

  val clientParameters = TLMasterPortParameters.v1(
    Seq(TLMasterParameters.v1(
      name = "icache",
      sourceId = IdRange(0, cacheParams.nMissEntries + 1), // n missEntries handle n req in mainPipe and "+ 1" means 1 channel id for FDIP prefetch
    )),
    requestFields = cacheParams.reqFields,
    echoFields = cacheParams.echoFields
  )

  val clientNode = TLClientNode(Seq(clientParameters))

  lazy val module = new ICacheImp(this)
}

// ICache: mainPipe+missUnit+metaArray+dataArray+FDIPPrefetch
class ICacheImp(outer: ICache) extends LazyModuleImp(outer) with HasICacheParameters {
  val io = IO(new ICacheIO)

  println("ICache:")
  println("  ICacheSets: " + cacheParams.nSets)
  println("  ICacheWays: " + cacheParams.nWays)
  println("  ICacheBanks: " + PortNumber)

  println("  enableICachePrefetch:     " + cacheParams.enableICachePrefetch)
  println("  prefetchToL1:       " + cacheParams.prefetchToL1)
  println("  prefetchPipeNum:    " + cacheParams.prefetchPipeNum)
  println("  nPrefetchEntries:   " + cacheParams.nPrefetchEntries)
  println("  nPrefBufferEntries: " + cacheParams.nPrefBufferEntries)
  println("  maxIPFMoveConf:     " + cacheParams.maxIPFMoveConf)

}
