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

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import wenxuan.common._

trait L1CacheParams {
  def nSets:         Int
  def nWays:         Int
  def rowBits:       Int
  def nTLBSets:      Int
  def nTLBWays:      Int
  def blockBytes:    Int // TODO this is ignored in favor of p(CacheBlockBytes) in BaseTile

  val pageSize: Int = 4 * 1024
}

trait HasL1CacheParameters extends HasTileParameters{
  val cacheParams: L1CacheParams

  def nSets = cacheParams.nSets
  def nWays = cacheParams.nWays
  def rowBits = cacheParams.rowBits
  def blockBytes = cacheParams.blockBytes
  def blockBits = blockBytes * 8
  def blockOffBits = log2Up(blockBytes)

  def idxBits = log2Up(nSets)
  def highestIdxBit = idxBits - 1
  def untagBits = idxBits + blockOffBits
  def pgIdxBits = 12 // 4K page
  def pgUntagBits = untagBits min pgIdxBits
  def tagBits = PAddrBits - pgUntagBits

  def get_phy_tag(paddr: UInt) = (paddr >> pgUntagBits).asUInt
  def get_idx(vaddr: UInt) = vaddr(untagBits-1, blockOffBits)
  def get_block(addr: UInt) = (addr >> blockOffBits).asUInt // block number

  def beatBits = l1BusDataWidth_Bits
  def beatBytes = beatBits / 8
  def refillCycles = blockBytes / beatBytes
}

abstract class L1CacheBundle(implicit p: Parameters) extends WXBundle
  with HasL1CacheParameters


// L1 Error infomation signal, downstream to L2 and handle in bus error unit
class L1BusErrorUnitInfo(implicit p: Parameters) extends WXBundle {
  val ecc_error = Valid(UInt(PAddrBits.W))
}

class L1CacheErrorInfo(implicit p: Parameters) extends WXBundle {
  // L1CacheErrorInfo is also used to encode customized CACHE_ERROR CSR
  val source = Output(new Bundle() {
    val tag = Bool() // l1 meta ecc error
    val data = Bool() // l1 data ecc error
    val l2 = Bool() // l2 cache cause the Error, happened in L1 miss-> L2
  })
  val opType = Output(new Bundle() { // specifies the operation that causes error
    val fetch = Bool()
    val load = Bool()
    val store = Bool()
    val probe = Bool()
    val release = Bool()
    val atom = Bool()
  })

  // report error and paddr to beu
  // bus error unit will receive error info if ecc_error.valid
  val report_to_beu = Output(Bool()) // ecc_error = meta ecc error || data ecc error
  val paddr = Output(UInt(PAddrBits.W)) // error paddr to beu

  // there is an valid error: two situation, l1 cache ECC error Or l1 miss->l2 meet error caused by l2
  // l1 cache error will always be report to CACHE_ERROR csr
  val valid = Output(Bool())

  def toL1BusErrorUnitInfo(): L1BusErrorUnitInfo = { // to BEU Information
    val beu_info = Wire(new L1BusErrorUnitInfo)
    beu_info.ecc_error.valid := report_to_beu
    beu_info.ecc_error.bits := paddr
    beu_info
  }
}

class L1CacheToCsrIO(implicit p: Parameters) extends WXBundle {
  val distribute_csr = Flipped(new DistributedCSRIO)
  val update = new DistributedCSRUpdateReq
}