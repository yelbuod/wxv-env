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

/*ICache externel interface bundle, parameters and top module*/
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
import wenxuan.cache.mmu.TlbRequestIO
import wenxuan.backend.fu.{PMPReqBundle,PMPRespBundle}

// 32KB, 4-way, 64-blockByte
case class ICacheParams(
  nSets: Int = 128,
  nWays: Int = 4,
  rowBits: Int = 64,
  nTLBSets: Int = 1,
  nTLBWays: Int = 32,
  blockBytes: Int = 64,
  nMissEntries: Int = 2,
  tagECC: Option[String] = None,
  dataECC: Option[String] = None,
  replacer: Option[String] = Some("random"),
  enableICachePrefetch: Boolean = true, // enable FDIP Prefetch module prefetch to L2
  prefetchToL1: Boolean = false, // FDIP Prefetch module move to L1 Cache meta/data
  prefetchPipeNum: Int = 1, // prefetch request number
  nPrefetchEntries: Int = 12, // prefetch Issue Queue entry number
  nPrefBufferEntries: Int = 32, // prefetch buffer entry number
  maxIPFMoveConf: Int = 1, // confidence threshold of prefetch buffer move to L1 Cache
  ICacheECCForceError: Boolean = false, // for ICache ECC test
) extends L1CacheParams {

  val setBytes = nSets * blockBytes
  val aliasBitsOpt = DCacheParameters().aliasBitsOpt //if(setBytes > pageSize) Some(log2Ceil(setBytes / pageSize)) else None
  val reqFields: Seq[BundleFieldBase] = Seq( // used to distinguish the source of request
    PrefetchField(), // whether the request is from prefetch
    ReqSourceField() // a enumeration (MemReqSource) specifies the request sources
  ) ++ aliasBitsOpt.map(AliasField)
  val echoFields: Seq[BundleFieldBase] = Nil

  def tagCode: Code = Code.fromString(tagECC)

  def dataCode: Code = Code.fromString(dataECC)

  def replacement = ReplacementPolicy.fromString(replacer, nWays, nSets)
}

trait HasICacheParameters extends HasL1CacheParameters {
  val cacheParams = tileParams.icache

  def PortNumber = 2 // icache support 2 request channel

  // nWays ICache is divided into partWayNum SRAM with pWay
  def partWayNum = 4
  def pWay = nWays / partWayNum
  // split a cache block into partBlockBits-bit parts
  // each part store in 2 banks SRAM respectively
  def partBlockBits = blockBits / 2
  // data ECC
  def dataCodeUnit = 16 // per dataCodeUnit-bit as a ECC encode unit in data block
  def encDataUnitBits = cacheParams.dataCode.width(dataCodeUnit) // width after encode
  def dataCodeBits = encDataUnitBits - dataCodeUnit // Code width per dataCodeUnit
  def dataCodeUnitNum = partBlockBits / dataCodeUnit // the number of ECC encode units in a partBlock
  def dataCodeEntryBits = dataCodeBits * dataCodeUnitNum // the total number of code bits needed for a partBlock

  def ICacheECCForceError = cacheParams.ICacheECCForceError

  // IPrefetch
  def enableICachePrefetch = cacheParams.enableICachePrefetch
  def prefetchToL1 = cacheParams.prefetchToL1
  def prefetchPipeNum = cacheParams.prefetchPipeNum
  def nPrefetchEntries = cacheParams.nPrefetchEntries // the number of IPrefetch Queue Entries
  def nPrefBufferEntries = cacheParams.nPrefBufferEntries // the number of IPrefetch Buffer Entries
  def maxIPFMoveConf = cacheParams.maxIPFMoveConf // IPrefetch Buffer move threshold

  def generatePipeControl(lastFire: Bool, thisFire: Bool, thisFlush: Bool, lastFlush: Bool): Bool = {
    val valid = RegInit(false.B)
    when(thisFlush) { valid := false.B }
    .elsewhen(lastFire && !lastFlush) { valid := true.B }
    .elsewhen(thisFire) { valid := false.B }
    valid
  }

  def addrAlignByte(addr: UInt, bytes: Int, highestOpt: Option[Int] = None): UInt = {
    val highest = if(highestOpt.isDefined) highestOpt.get else addr.getWidth
    Cat(addr(highest-1, log2Ceil(bytes)), 0.U(log2Ceil(bytes).W))
  }

  def InitQueue[T <: Data](entry: T, size: Int): Vec[T] = {
    return RegInit(VecInit(Seq.fill(size)(0.U.asTypeOf(entry.cloneType))))
  }
}

abstract class ICacheBundle(implicit p: Parameters) extends WXBundle
  with HasICacheParameters

abstract class ICacheModule(implicit p: Parameters) extends WXModule
  with HasICacheParameters

abstract class ICacheArray(implicit p: Parameters) extends WXModule
  with HasICacheParameters

class ICachePMPBundle(implicit p: Parameters) extends ICacheBundle{
  val req  = Valid(new PMPReqBundle())
  val resp = Input(new PMPRespBundle())
}

class ICache()(implicit p: Parameters) extends LazyModule
  with HasICacheParameters
{
  override def shouldBeInlined: Boolean = false
  // outer node
  val clientParameters = TLMasterPortParameters.v1(
    Seq(TLMasterParameters.v1(
      name = "icache",
      sourceId = IdRange(0, cacheParams.nMissEntries + 1), // n missEntries handle n req in mainPipe and "+ 1" means 1 channel id for FDIP prefetch
    )),
    requestFields = cacheParams.reqFields,
    echoFields = cacheParams.echoFields
  )

  val clientNode = TLClientNode(Seq(clientParameters))
  // module implementation : submodule interconnect and connect with outer node
  lazy val module = new ICacheImp(this)
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
  // val perfInfo    = Output(new ICachePerfInfo)
  val error       = new L1CacheErrorInfo
  val fencei      = Input(Bool())
  /* Cache Instruction */
  val csr         = new L1CacheToCsrIO
  /* CSR control signal */
  val csr_pf_enable = Input(Bool())
  val csr_parity_enable = Input(Bool())
}

// ICache: mainPipe+missUnit+metaArray+dataArray+FDIPPrefetch
// The implicit variable [[p]] inside the LazyModuleImp
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


  val (bus, edge) = outer.clientNode.out.head

  val metaArray = Module(new ICacheMetaArray)
  val dataArray = Module(new ICacheDataArray)
  val prefetchMetaArray = Module(new ICacheMetaArrayNoBanked)
  val mainPipe = Module(new ICacheMainPipe)
  val missUnit = Module(new ICacheMissUnit(edge))
  val fdipPrefetch = Module(new FDIPPrefetch(edge))

  fdipPrefetch.io.hartId := io.hartId
  fdipPrefetch.io.fencei := io.fencei
  fdipPrefetch.io.ftqReq <> io.prefetch
  fdipPrefetch.io.metaReadReq <> prefetchMetaArray.io.read
  fdipPrefetch.io.metaReadResp <> prefetchMetaArray.io.readResp
  fdipPrefetch.io.ICacheMissUnitInfo <> missUnit.io.ICacheMissUnitInfo
  fdipPrefetch.io.ICacheMainPipeInfo <> mainPipe.io.ICacheMainPipeInfo
  fdipPrefetch.io.IPFBufferRead <> mainPipe.io.IPFBufferRead
  fdipPrefetch.io.IPFReplacer <> mainPipe.io.IPFReplacer
  fdipPrefetch.io.IPQRead <> mainPipe.io.IPQRead
  fdipPrefetch.io.metaWrite <> DontCare
  fdipPrefetch.io.dataWrite <> DontCare

  // Meta Array. Priority: missUnit > fdipPrefetch
  if (prefetchToL1) {
    val meta_write_arb = Module(new Arbiter(new ICacheMetaWriteBundle(), 2))
    meta_write_arb.io.in(0) <> missUnit.io.meta_write
    meta_write_arb.io.in(1) <> fdipPrefetch.io.metaWrite
    meta_write_arb.io.out <> metaArray.io.write
    // prefetch Meta Array. Connect meta_write_arb to ensure the data is same as metaArray
    prefetchMetaArray.io.write <> meta_write_arb.io.out
  } else {
    missUnit.io.meta_write <> metaArray.io.write
    missUnit.io.meta_write <> prefetchMetaArray.io.write
    // ensure together wirte to metaArray and prefetchMetaArray
    missUnit.io.meta_write.ready := metaArray.io.write.ready && prefetchMetaArray.io.write.ready
  }

  // Data Array. Priority: missUnit > fdipPrefetch
  if (prefetchToL1) {
    val data_write_arb = Module(new Arbiter(new ICacheDataWriteBundle(), 2))
    data_write_arb.io.in(0) <> missUnit.io.data_write
    data_write_arb.io.in(1) <> fdipPrefetch.io.dataWrite
    data_write_arb.io.out <> dataArray.io.write
  } else {
    missUnit.io.data_write <> dataArray.io.write
  }

  mainPipe.io.dataArray.toIData <> dataArray.io.read
  mainPipe.io.dataArray.fromIData <> dataArray.io.readResp
  mainPipe.io.metaArray.toIMeta <> metaArray.io.read
  mainPipe.io.metaArray.fromIMeta <> metaArray.io.readResp
  mainPipe.io.metaArray.fromIMeta <> metaArray.io.readResp
  mainPipe.io.respStall := io.stop
  mainPipe.io.csr_parity_enable := io.csr_parity_enable
  mainPipe.io.hartId := io.hartId

  io.pmp(0) <> mainPipe.io.pmp(0)
  io.pmp(1) <> mainPipe.io.pmp(1)
  io.pmp(2) <> fdipPrefetch.io.pmp

  io.itlb(0) <> mainPipe.io.itlb(0)
  io.itlb(1) <> mainPipe.io.itlb(1)
  io.itlb(2) <> fdipPrefetch.io.iTLBInter

  //notify IFU that Icache pipeline is available
  io.toIFU := mainPipe.io.fetch.req.ready
  // io.perfInfo := mainPipe.io.perfInfo

  io.fetch.resp <> mainPipe.io.fetch.resp
  io.fetch.topdownIcacheMiss := mainPipe.io.fetch.topdownIcacheMiss
  io.fetch.topdownItlbMiss := mainPipe.io.fetch.topdownItlbMiss

  for (i <- 0 until PortNumber) {
    missUnit.io.req(i) <> mainPipe.io.mshr(i).toMSHR
    mainPipe.io.mshr(i).fromMSHR <> missUnit.io.resp(i)
  }

  missUnit.io.hartId := io.hartId
  missUnit.io.fencei := io.fencei
  missUnit.io.fdip_acquire <> fdipPrefetch.io.mem_acquire
  missUnit.io.fdip_grant <> fdipPrefetch.io.mem_grant

  bus.b.ready := false.B
  bus.c.valid := false.B
  bus.c.bits := DontCare
  bus.e.valid := false.B
  bus.e.bits := DontCare

  bus.a <> missUnit.io.mem_acquire

  // connect bus d
  missUnit.io.mem_grant.valid := false.B
  missUnit.io.mem_grant.bits := DontCare

  //Parity error port
  val errors = mainPipe.io.errors
  io.error <> RegNext(Mux1H(errors.map(e => e.valid -> e)))


  mainPipe.io.fetch.req <> io.fetch.req
  bus.d.ready := false.B
  missUnit.io.mem_grant <> bus.d

  // fencei connect
  metaArray.io.fencei := io.fencei
  prefetchMetaArray.io.fencei := io.fencei

//  val perfEvents = Seq(
//    ("icache_miss_cnt  ", false.B),
//    ("icache_miss_penalty", BoolStopWatch(start = false.B, stop = false.B || false.B, startHighPriority = true)),
//  )
//  generatePerfEvent()

  // Customized csr cache op support
//  val cacheOpDecoder = Module(new CSRCacheOpDecoder("icache", CacheInstrucion.COP_ID_ICACHE))
//  cacheOpDecoder.io.csr <> io.csr
//  dataArray.io.cacheOp.req := cacheOpDecoder.io.cache.req
//  metaArray.io.cacheOp.req := cacheOpDecoder.io.cache.req
//  prefetchMetaArray.io.cacheOp.req := cacheOpDecoder.io.cache.req
//  cacheOpDecoder.io.cache.resp.valid :=
//    dataArray.io.cacheOp.resp.valid ||
//      metaArray.io.cacheOp.resp.valid
//  cacheOpDecoder.io.cache.resp.bits := Mux1H(List(
//    dataArray.io.cacheOp.resp.valid -> dataArray.io.cacheOp.resp.bits,
//    metaArray.io.cacheOp.resp.valid -> metaArray.io.cacheOp.resp.bits,
//  ))
//  cacheOpDecoder.io.error := io.error
//  assert(!((dataArray.io.cacheOp.resp.valid +& metaArray.io.cacheOp.resp.valid) > 1.U))

}
