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

/*ICache inner module interface bundle*/
package wenxuan.frontend.icache

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters

/** ICache Interface to MetaArray and DataArray */
class ICacheReadBundle(implicit p: Parameters) extends ICacheBundle
{
  val isDoubleLine  = Bool()
  val vSetIdx       = Vec(2,UInt(idxBits.W))
}

class ICacheMetaRespBundle(implicit p: Parameters) extends ICacheBundle
{
  val metaData   = Vec(2, Vec(nWays, new ICacheMetadata))
  val errors     = Vec(2, Vec(nWays ,Bool() ))
  val entryValid = Vec(2, Vec(nWays, Bool()))

  def tags = VecInit(metaData.map(port => VecInit(port.map( way=> way.tag ))))
}

class ICacheDataRespBundle(implicit p: Parameters) extends ICacheBundle
{
  val datas = Vec(2, Vec(nWays, UInt((blockBits/2).W)))
  val codes = Vec(2, Vec(nWays, UInt(dataCodeEntryBits.W)))
}

class ICacheMetaWriteBundle(implicit p: Parameters) extends ICacheBundle
{
  val phyTag  = UInt(tagBits.W)
  val virIdx  = UInt(idxBits.W)
  val waymask = UInt(nWays.W)
  val bankIdx = Bool()

  def generate(tag:UInt, idx:UInt, waymask:UInt, bankIdx: Bool){
    this.phyTag  := tag
    this.virIdx  := idx
    this.waymask := waymask
    this.bankIdx   := bankIdx
  }
}

class ICacheDataWriteBundle(implicit p: Parameters) extends ICacheBundle
{
  val data = UInt(blockBits.W)
  val virIdx = UInt(idxBits.W)
  val waymask = UInt(nWays.W)
  val bankIdx = Bool()
  /** just for the recent write record, no used in real array write */
  val paddr = UInt(PAddrBits.W)

  def generate(data:UInt, idx:UInt, waymask:UInt, bankIdx:Bool, paddr:UInt) = {
    this.data := data
    this.virIdx := idx
    this.waymask := waymask
    this.bankIdx := bankIdx
    this.paddr := paddr
  }
}

/** ICache MainPipe <> IPrefetch Buffer */
class IPFBufferRead(implicit p: Parameters) extends IPrefetchBundle
{
  /** IPrefetch Buffer input */
  val req = Vec(PortNumber, Flipped(DecoupledIO(new Bundle {
    val paddr   = UInt(PAddrBits.W)
  })))
  /** IPrefetch Buffer output */
  val resp = Vec(PortNumber, Output(new Bundle {
    val ipf_hit   = Bool()
    val cacheline = UInt(blockBits.W)
  }))
}

/** ICache MainPipe <> IPrefetch Queue */
class IPQRead(implicit p: Parameters) extends IPrefetchBundle
{
  /** IPrefetch Queue input */
  val req = Vec(PortNumber, Flipped(DecoupledIO(new Bundle {
    val paddr = UInt(PAddrBits.W)
  })))
  /** IPrefetch Queue output */
  val resp = Vec(PortNumber, Output(new Bundle {
    val ipq_hit = Bool()
    val cacheline = UInt(blockBits.W)
    val data_valid = Bool()
  }))
}

/** Replacer Info to IPrefetch for move buffer entry to replace meta/data entry */
class IPFReplacer(implicit p: Parameters) extends IPrefetchBundle
{
  // IPrefetch Buffer output the set to be replaced
  val vsetIdx = Output(UInt(idxBits.W))
  // Replacer arbitration and return one-hot code used to select a way to replace
  val waymask = Input(UInt(nWays.W))
}

/** MainPipe <> MissUnit */
class ICacheMissReq(implicit p: Parameters) extends ICacheBundle
{
  val paddr = UInt(PAddrBits.W) // to low-level refill request
  // used to refill to meta/data Array
  val vSetIdx = UInt(idxBits.W)
  val waymask = UInt(nWays.W)

  def getPhyTag = get_phy_tag(paddr)
}

class ICacheMissResp(implicit p: Parameters) extends ICacheBundle
{
  val dataline = UInt(blockBits.W)
  val corrupt = Bool()
}

/** MainPipe / MissUnit req info to IPrefetch for filter */
class FilterInfo(implicit p: Parameters) extends ICacheBundle{
  val paddr = UInt(PAddrBits.W)
  val valid = Bool()
}

class MissSlotInfo(implicit p: Parameters) extends ICacheBundle{
  val ptag    = UInt(tagBits.W)
  val vSetIdx = UInt(idxBits.W)
  val valid   = Bool()
}

// mainpipe requests info to IPrefetch to filter the same req in the prefetch
class ICacheMainPipeInfo(implicit p: Parameters) extends IPrefetchBundle{
  val s1Info = Output(Vec(PortNumber, new FilterInfo))
  val s2Info = Output(Vec(PortNumber, new FilterInfo))
  val missSlot = Output(Vec(PortNumber, new MissSlotInfo))
}

// missUnit to IPrefetch to filter the same req in the prefetch
class ICacheMissUnitInfo(implicit p: Parameters) extends IPrefetchBundle{
  val mshr        = Output(Vec(PortNumber, new FilterInfo))
  val recentWrite = Output(Vec(2, new FilterInfo))
}