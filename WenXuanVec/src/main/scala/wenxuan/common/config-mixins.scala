/***************************************************************************************
 * Copyright (c) 2024-2026 YangYang, https://github.com/yelbuod
 *
 * XiangShan is licensed under Mulan PSL v2.
 * You can use this software according to the terms and conditions of the Mulan PSL v2.
 * You may obtain a copy of Mulan PSL v2 at:
 *          http://license.coscl.org.cn/MulanPSL2
 *
 * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
 * EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
 * MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
 *
 * See the Mulan PSL v2 for more details.
 *
 * Description:
 * 	Configure core parameters, and decoupled the predictor parameters from other parameters within the core
 ***************************************************************************************/

package wenxuan.common

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.{Parameters, Config, Field}

class WithDefaultWenXuan extends Config(
	new WithTAGEBPD ++ // Default to TAGE BPD
	new WithBaseWXNoBPD
)

class WithBaseWXNoBPD extends Config((site, here, up) => {
	case WXVTileKey => WXVTileParams()
})