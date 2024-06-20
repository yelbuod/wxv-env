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

package wenxuan.frontend

import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import org.chipsalliance.cde.config.Parameters
import wenxuan.common.{FrontendToCtrlIO, HasWXCommonParameters}
import wenxuan.frontend.icache.{HasICacheParameters, ICache}
import wenxuan.cache.L1CacheErrorInfo
import wenxuan.cache.mmu.TLB

trait HasFrontendParameters extends HasICacheParameters {
  val itlbParams = cacheParams.itlbParams
  def ICacheTlbPortNum = PortNumber + prefetchPipeNum
  def ICachePmpPortNum = PortNumber + prefetchPipeNum
  def IfuTlbPortNum = 1 // for mmio
  def IfuPmpPortNum = 1 // for mmio

  def itlbPortNum = ICacheTlbPortNum + IfuTlbPortNum
  def ipmpPortNum = ICachePmpPortNum + IfuPmpPortNum

  def itlbPortBlock = Seq.fill(PortNumber)(false) ++
                                    Seq.fill(prefetchPipeNum)(false) ++
                                    Seq.fill(IfuTlbPortNum)(true)
}

class Frontend()(implicit p: Parameters) extends LazyModule
  with HasFrontendParameters
{
  override def shouldBeInlined: Boolean = false

  // outer submodule
  val icache = LazyModule(new ICache())

  // module implementation : submodule interconnect and connect with outer submodule (icache)
  lazy val module = new FrontendImp(this)
}

class FrontendImp(outer: Frontend) extends LazyModuleImp(outer)
  with HasFrontendParameters
{
  val io = IO(new Bundle {
    val backend = new FrontendToCtrlIO
    val error = new L1CacheErrorInfo
  })

  private val icache = outer.icache.module

  io.error <> RegNext(RegNext(icache.io.error))

  /** Ftq */
  val ftq = Module(new Ftq)
  icache.io.prefetch <> ftq.io.toPrefetch
  icache.io.fetch.req <> ftq.io.toICache.req
  io.backend.fromFtq <> ftq.io.toBackend
  ftq.io.fromBackend <> io.backend.toFtq

  /** ITLB   */
  val itlb = Module(new TLB(itlbPortNum, 1, itlbPortBlock, itlbParams))
  itlb.io.requestor.take(ICacheTlbPortNum) zip icache.io.itlb foreach {
    case (itlb_io, icache_itlb_io) => itlb_io <> icache_itlb_io
  }

  /** IBuffer */
  val ibuffer = Module(new IBuffer)
  io.backend.instPackVec <> ibuffer.io.out
}
