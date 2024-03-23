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
import wenxuan.common.HasWXCommonParameters
import wenxuan.frontend.icache.ICache
import wenxuan.cache.L1CacheErrorInfo

class Frontend()(implicit p: Parameters) extends LazyModule
  with HasWXCommonParameters
{
  override def shouldBeInlined: Boolean = false

  // outer submodule
  val icache = LazyModule(new ICache())

  // module implementation : submodule interconnect and connect with outer submodule (icache)
  lazy val module = new FrontendImp(this)
}

class FrontendImp(outer: Frontend) extends LazyModuleImp(outer)
  with HasWXCommonParameters
{
  val io = IO(new Bundle {
    val error = new L1CacheErrorInfo
  })

  private val icache = outer.icache.module

  io.error <> RegNext(RegNext(icache.io.error))
}
