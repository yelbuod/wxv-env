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

package wxtop

import org.chipsalliance.cde.config.Config
import freechips.rocketchip.tile.XLen
import system.{SoCParameters, SoCParamsKey}
import xiangshan.{DebugOptions, DebugOptionsKey, XSCoreParameters, XSCoreParamsKey, XSTileKey}
import wenxuan.common.{PMParams, PMParamsKey, WithDefaultWenXuan}

import scala.collection.Seq

class BaseConfig extends Config(
  new WithDefaultWenXuan ++
    new Config((site, here, up) => {
      case XLen => 64
      case DebugOptionsKey => DebugOptions()
      case SoCParamsKey => SoCParameters()
      case PMParamsKey => PMParams()
//      case XSTileKey => Seq.tabulate(1){ i => XSCoreParameters(HartId = i) }
//      case XSCoreParamsKey => XSCoreParameters(HartId = 0)
    })
)

class DefaultConfig extends Config(
  new BaseConfig()
)
