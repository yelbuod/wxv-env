/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
* Copyright (c) 2020-2021 Peng Cheng Laboratory
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
***************************************************************************************/

package wxtop

import chisel3._
import chisel3.util._
import xiangshan.DebugOptionsKey
//import utils._
//import huancun.{HCCacheParameters, HCCacheParamsKey, HuanCun, PrefetchRecv, TPmetaResp}
import utility._
import system.HasSoCParameter
//import device._
import chisel3.stage.ChiselGeneratorAnnotation
import org.chipsalliance.cde.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.jtag.JTAGIO
import wenxuan.common._
import top.Generator
import huancun.{HCCacheParameters, HCCacheParamsKey, HuanCun, PrefetchRecv, TPmetaResp}

trait HasWXSoCParameter extends HasSoCParameter {
  val wxvtiles = Seq(p(WXVTileKey))
}

class WXVTop()(implicit p: Parameters) extends LazyModule with HasWXSoCParameter
{
//  ResourceBinding {
//    val width = ResourceInt(2)
//    val model = "freechips,rocketchip-unknown"
//    Resource(ResourceAnchors.root, "model").bind(ResourceString(model))
//    Resource(ResourceAnchors.root, "compat").bind(ResourceString(model + "-dev"))
//    Resource(ResourceAnchors.soc, "compat").bind(ResourceString(model + "-soc"))
//    Resource(ResourceAnchors.root, "width").bind(width)
//    Resource(ResourceAnchors.soc, "width").bind(width)
//    Resource(ResourceAnchors.cpus, "width").bind(ResourceInt(1))
//    def bindManagers(xbar: TLNexusNode) = {
//      ManagerUnification(xbar.edges.in.head.manager.managers).foreach{ manager =>
//        manager.resources.foreach(r => r.bind(manager.toResource))
//      }
//    }
//    bindManagers(misc.l3_xbar.asInstanceOf[TLNexusNode])
//    bindManagers(misc.peripheralXbar.asInstanceOf[TLNexusNode])
//  }

//  println(s"FPGASoC cores: $NumCores banks: $L3NBanks block size: $L3BlockSize bus size: $L3OuterBusWidth")

  val core_with_l2 = LazyModule(new WXVTile())

  class WXVTopImp(wrapper: LazyModule) extends LazyRawModuleImp(wrapper) {

  }

  lazy val module = new WXVTopImp(this)
}

object TopMain extends App {
  val (config, firrtlOpts, firtoolOpts) = ArgParser.parse(args)

  // tools: init to close dpi-c when in fpga
  val envInFPGA = config(DebugOptionsKey).FPGAPlatform
  val enableChiselDB = config(DebugOptionsKey).EnableChiselDB
  val enableConstantin = config(DebugOptionsKey).EnableConstantin
//  Constantin.init(enableConstantin && !envInFPGA)
//  ChiselDB.init(enableChiselDB && !envInFPGA)

  val soc = DisableMonitors(p => LazyModule(new WXVTop()(p)))(config)
  Generator.execute(firrtlOpts, soc.module, firtoolOpts)
  FileRegisters.write(fileDir = "./build", filePrefix = "WXVTop.")
}
