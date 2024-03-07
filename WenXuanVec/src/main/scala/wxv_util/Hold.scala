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

package wxv_util

import chisel3._
import chisel3.util._

object ResultHoldBypass {
  def apply[T <: Data](data: T, valid: Bool, init: Option[T] = None): T = {
    val hold_data = if (init.isDefined) RegEnable(data, init.get, valid) else RegEnable(data, valid)
    Mux(valid, data, hold_data)
  }
}

object ValidHoldBypass{
  def apply(infire: Bool, outfire: Bool, flush: Bool = false.B) = {
    val valid = RegInit(false.B)
    when (flush) { valid := false.B } // NOTE: the flush will flush in & out
    .elsewhen (outfire) { valid := false.B } // ATTENTION: order different with ValidHold
    .elsewhen (infire) { valid := true.B }
    valid || infire
  }
}
