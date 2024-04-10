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

package wenxuan.backend.fu

import chisel3._
import chisel3.util._

trait HasCSRConst {

  /**
   * CSR Mapping Address Constants
   */
  /** Physical Memory Protection */
  val PmpcfgBase = 0x3A0
  val PmpaddrBase = 0x3B0
  /** Physical Memory Attribution */
  val PmacfgBase = 0x7C0
  val PmaaddrBase =0x7C8

  /**
   * Privilege level
   */
  def ModeM = 0x3.U
  def ModeH = 0x2.U
  def ModeS = 0x1.U
  def ModeU = 0x0.U
}
