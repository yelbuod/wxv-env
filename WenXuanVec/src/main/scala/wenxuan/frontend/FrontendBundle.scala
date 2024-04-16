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

/** Ftq request to ICache Prefetch */
class PrefetchRequest(implicit p: Parameters) extends WXBundle {
  val target          = UInt(VAddrBits.W)
}

class FtqPrefechBundle(implicit p: Parameters) extends WXBundle {
  val req = DecoupledIO(new PrefetchRequest)
}

/** Ftq request to ICache fetch */
class FtqICacheInfo(implicit p: Parameters)extends WXBundle with HasICacheParameters{
  val startAddr           = UInt(VAddrBits.W)
  val nextlineStart       = UInt(VAddrBits.W)
  // the default fetchWidth is half of the blockBytes, so when startAddr is in the middle of the cacheline,
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

/** BPU resp prediction info to Ftq */
class BranchPredictionBundle(implicit p: Parameters) extends WXBundle
  with HasBPUConst with BPUUtils
{
  val pc    = Vec(numDup, UInt(VAddrBits.W))
  val valid = Vec(numDup, Bool())
  val hasRedirect  = Vec(numDup, Bool())
  val ftq_idx = new FtqPtr
}

class BranchPredictionResp(implicit p: Parameters) extends WXBundle with HasBPUConst {
  // val valids = Vec(3, Bool())
  val s1 = new BranchPredictionBundle
  val s2 = new BranchPredictionBundle
  val s3 = new BranchPredictionBundle

  val last_stage_meta = UInt(MaxMetaLength.W)
  val last_stage_spec_info = new Ftq_Redirect_SRAMEntry
  val last_stage_ftb_entry = new FTBEntry

  val topdown_info = new FrontendTopDownBundle

  def lastStage = s3
  def selectedResp ={
    val res =
      PriorityMux(Seq(
        ((s3.valid(3) && s3.hasRedirect(3)) -> s3),
        ((s2.valid(3) && s2.hasRedirect(3)) -> s2),
        (s1.valid(3) -> s1)
      ))
    res
  }
  def selectedRespIdxForFtq =
    PriorityMux(Seq(
      ((s3.valid(3) && s3.hasRedirect(3)) -> BP_S3),
      ((s2.valid(3) && s2.hasRedirect(3)) -> BP_S2),
      (s1.valid(3) -> BP_S1)
    ))
}

class BpuToFtqIO(implicit p: Parameters) extends WXBundle {
  val resp = DecoupledIO(new BranchPredictionResp())
}