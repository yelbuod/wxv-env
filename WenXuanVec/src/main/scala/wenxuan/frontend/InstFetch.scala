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
 ***************************************************************************************/

package wenxuan.frontend

import chisel3._
import chisel3.util._
import wenxuan.mem._

//class InstFetch extends Module {
//  val io = IO(new Bundle {
//    val imem = new RomIO
//    val pc = Output(UInt(64.W))
//    val inst = Output(UInt(32.W))
//    val pc_jaddr = Flipped(Valid(UInt(64.W)))
//    val pc_en = Output(Bool())
//  })
//
//  val pc_en = RegInit(false.B)
//  pc_en := true.B
//
//  val pc = RegInit("h80000000".U(64.W))
//  val pc_next = Mux(io.pc_jaddr.valid, io.pc_jaddr.bits, pc + 4.U)
//  pc := pc_next
//
//  io.imem.en := true.B
//  io.imem.addr := pc.asUInt()
//
//  io.pc := pc
//  io.inst := io.imem.rdata(31, 0)
//  io.pc_en := pc_en
//}
