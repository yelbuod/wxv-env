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
import org.chipsalliance.cde.config.Parameters
import wenxuan.common.{WXBundle, WXModule}

class Ftq_RF_Components(implicit p: Parameters) extends WXBundle{
  val startAddr = UInt(VAddrBits.W)
  val nextLineAddr = UInt(VAddrBits.W)

}

class Ftq(implicit p: Parameters) extends WXModule
{
  val io = IO(new Bundle {
    val fromBpu = Flipped(new BpuToFtqIO)
    val fromIfu = Flipped(new IfuToFtqIO)

    val toBpu = new FtqToBpuIO
    val toIfu = new FtqToIfuIO

    val toICache = new FtqToICacheIO
    val toPrefetch = new FtqPrefechBundle

    val fromBackend = Flipped(new CtrlToFtqIO)
    val toBackend = new FtqToCtrlIO


    val bpuInfo = new Bundle {
      val bpRight = Output(UInt(XLEN.W))
      val bpWrong = Output(UInt(XLEN.W))
    }

    val mmioCommitRead = Flipped(new mmioCommitRead)

    // for perf
    val ControlBTBMissBubble = Output(Bool())
    val TAGEMissBubble = Output(Bool())
    val SCMissBubble = Output(Bool())
    val ITTAGEMissBubble = Output(Bool())
    val RASMissBubble = Output(Bool())
  })
  io.bpuInfo := DontCare

}
