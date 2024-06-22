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

package wenxuan.backend.rob

import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import org.chipsalliance.cde.config.Parameters
import wenxuan.common.{HasWXCommonParameters, WXVTileKey}
import utility.CircularQueuePtr
import wenxuan.backend.{RobCommitIO, SnapshotPort}

class RobPtr(implicit val p: Parameters) extends CircularQueuePtr[RobPtr](
  p => p(WXVTileKey).core.numRobEntries
) {

}

class Rob(implicit p: Parameters) extends LazyModule with HasWritebackSink with HasWXCommonParameters {
  override def shouldBeInlined: Boolean = false

  lazy val module = new RobImp(this)
}

class RobImp(outer: Rob)(implicit p: Parameters) extends LazyModuleImp(outer){
  val io = IO(new Bundle{
    val commits = Output(new RobCommitIO()) // Commit & WALK
    val snpt = Input(new SnapshotPort()) // snapshot info
  })
}
