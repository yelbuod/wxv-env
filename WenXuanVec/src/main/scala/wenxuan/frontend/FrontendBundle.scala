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
import wenxuan.common.WXBundle
import org.chipsalliance.cde.config.Parameters
import wenxuan.frontend.icache.HasICacheParameters

// Ftq request to ICache Prefetch
class PrefetchRequest(implicit p: Parameters) extends WXBundle {
  val target          = UInt(VAddrBits.W)
}

class FtqPrefechBundle(implicit p: Parameters) extends WXBundle {
  val req = DecoupledIO(new PrefetchRequest)
}

// Ftq request to ICache fetch
class FtqICacheInfo(implicit p: Parameters)extends WXBundle with HasICacheParameters{
  val startAddr           = UInt(VAddrBits.W)
  val nextlineStart       = UInt(VAddrBits.W)
  // when startAddr is in the middle of the cacheline,
  //  crossCacheline need to be set, means that the next line request is valid
  def crossCacheline =  startAddr(blockOffBits - 1) === 1.U
  def fromFtqPcBundle(b: Ftq_RF_Components) = {
    this.startAddr := b.startAddr
    this.nextlineStart := b.nextLineAddr
    this
  }
}

class FtqToICacheRequestBundle(implicit p: Parameters)extends WXBundle with HasICacheParameters{
  val pcMemRead           = Vec(5, new FtqICacheInfo)
  val readValid           = Vec(5, Bool())
}