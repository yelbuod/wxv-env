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

package wenxuan.cache.mmu

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._


class TlbSectorEntry(pageNormal: Boolean, pageSuper: Boolean)(implicit p: Parameters) extends TlbBundle {
  require(pageNormal || pageSuper)

  val tag = if (!pageNormal) UInt((vpnLen - vpnnLen).W)
  else UInt(sectorvpnLen.W)
  val asid = UInt(AsidLen.W)
  val level = if (!pageNormal) Some(UInt(1.W))
  else if (!pageSuper) None
  else Some(UInt(2.W))
  val ppn = if (!pageNormal) UInt((ppnLen - vpnnLen).W)
  else UInt(sectorppnLen.W)
  val perm = new TlbSectorPermBundle
  val valididx = Vec(tlbcontiguous, Bool())
  val pteidx = Vec(tlbcontiguous, Bool())
  val ppn_low = Vec(tlbcontiguous, UInt(sectortlbwidth.W))

  /** level usage:
   *  !PageSuper: page is only normal, level is None, match all the tag
   *  !PageNormal: page is only super, level is a Bool(), match high 9*2 parts
   *  bits0  0: need mid 9bits
   *         1: no need mid 9bits
   *  PageSuper && PageNormal: page hold all the three type,
   *  bits0  0: need low 9bits
   *  bits1  0: need mid 9bits
   */

  def hit(vpn: UInt, asid: UInt, nSets: Int = 1, ignoreAsid: Boolean = false): Bool = {
    val asid_hit = if (ignoreAsid) true.B else (this.asid === asid)
    val addr_low_hit = valididx(vpn(2, 0))

    // NOTE: for timing, dont care low set index bits at hit check
    //       do not need store the low bits actually
    if (!pageSuper) asid_hit && drop_set_equal(vpn(vpn.getWidth - 1, sectortlbwidth), tag, nSets) && addr_low_hit
    else if (!pageNormal) {
      val tag_match_hi = tag(vpnnLen * 2 - 1, vpnnLen) === vpn(vpnnLen * 3 - 1, vpnnLen * 2)
      val tag_match_mi = tag(vpnnLen - 1, 0) === vpn(vpnnLen * 2 - 1, vpnnLen)
      val tag_match = tag_match_hi && (level.get.asBool || tag_match_mi)
      asid_hit && tag_match && addr_low_hit
    }
    else {
      val tmp_level = level.get
      val tag_match_hi = tag(vpnnLen * 3 - sectortlbwidth - 1, vpnnLen * 2 - sectortlbwidth) === vpn(vpnnLen * 3 - 1, vpnnLen * 2)
      val tag_match_mi = tag(vpnnLen * 2 - sectortlbwidth - 1, vpnnLen - sectortlbwidth) === vpn(vpnnLen * 2 - 1, vpnnLen)
      val tag_match_lo = tag(vpnnLen - sectortlbwidth - 1, 0) === vpn(vpnnLen - 1, sectortlbwidth) // if pageNormal is false, this will always be false
      val tag_match = tag_match_hi && (tmp_level(1) || tag_match_mi) && (tmp_level(0) || tag_match_lo)
      asid_hit && tag_match && addr_low_hit
    }
  }

  def wbhit(data: PtwSectorResp, asid: UInt, nSets: Int = 1, ignoreAsid: Boolean = false): Bool = {
    val vpn = Cat(data.entry.tag, 0.U(sectortlbwidth.W))
    val asid_hit = if (ignoreAsid) true.B else (this.asid === asid)
    val vpn_hit = Wire(Bool())
    val index_hit = Wire(Vec(tlbcontiguous, Bool()))

    // NOTE: for timing, dont care low set index bits at hit check
    //       do not need store the low bits actually
    if (!pageSuper) {
      vpn_hit := asid_hit && drop_set_equal(vpn(vpn.getWidth - 1, sectortlbwidth), tag, nSets)
    }
    else if (!pageNormal) {
      val tag_match_hi = tag(vpnnLen * 2 - 1, vpnnLen - sectortlbwidth) === vpn(vpnnLen * 3 - 1, vpnnLen * 2)
      val tag_match_mi = tag(vpnnLen - 1, 0) === vpn(vpnnLen * 2 - 1, vpnnLen)
      val tag_match = tag_match_hi && (level.get.asBool || tag_match_mi)
      vpn_hit := asid_hit && tag_match
    }
    else {
      val tmp_level = level.get
      val tag_match_hi = tag(vpnnLen * 3 - sectortlbwidth - 1, vpnnLen * 2 - sectortlbwidth) === vpn(vpnnLen * 3 - 1, vpnnLen * 2)
      val tag_match_mi = tag(vpnnLen * 2 - sectortlbwidth - 1, vpnnLen - sectortlbwidth) === vpn(vpnnLen * 2 - 1, vpnnLen)
      val tag_match_lo = tag(vpnnLen - sectortlbwidth - 1, 0) === vpn(vpnnLen - 1, sectortlbwidth) // if pageNormal is false, this will always be false
      val tag_match = tag_match_hi && (tmp_level(1) || tag_match_mi) && (tmp_level(0) || tag_match_lo)
      vpn_hit := asid_hit && tag_match
    }

    for (i <- 0 until tlbcontiguous) {
      index_hit(i) := data.valididx(i) && valididx(i)
    }

    // For example, tlb req to page cache with vpn 0x10
    // At this time, 0x13 has not been paged, so page cache only resp 0x10
    // When 0x13 refill to page cache, previous item will be flushed
    // Now 0x10 and 0x13 are both valid in page cache
    // However, when 0x13 refill to tlb, will trigger multi hit
    // So will only trigger multi-hit when PopCount(data.valididx) = 1
    vpn_hit && index_hit.reduce(_ || _) && PopCount(data.valididx) === 1.U
  }

  def apply(item: PtwSectorResp, asid: UInt): TlbSectorEntry = {
    this.tag := {if (pageNormal) item.entry.tag else item.entry.tag(sectorvpnLen - 1, vpnnLen - sectortlbwidth)}
    this.asid := asid
    val inner_level = item.entry.level.getOrElse(0.U)
    this.level.map(_ := { if (pageNormal && pageSuper) MuxLookup(inner_level, 0.U, Seq(
      0.U -> 3.U,
      1.U -> 1.U,
      2.U -> 0.U ))
    else if (pageSuper) ~inner_level(0)
    else 0.U })
    this.ppn := { if (!pageNormal) item.entry.ppn(sectorppnLen - 1, vpnnLen - sectortlbwidth)
    else item.entry.ppn }
    this.perm.apply(item)
    this.ppn_low := item.ppn_low
    this.valididx := item.valididx
    this.pteidx := item.pteidx
    this
  }

  // 4KB is normal entry, 2MB/1GB is considered as super entry
  def is_normalentry(): Bool = {
    if (!pageSuper) { true.B }
    else if (!pageNormal) { false.B }
    else { level.get === 0.U }
  }

  def genPPN(saveLevel: Boolean = false, valid: Bool = false.B)(vpn: UInt) : UInt = {
    val inner_level = level.getOrElse(0.U)
    val ppn_res = if (!pageSuper) Cat(ppn, ppn_low(vpn(sectortlbwidth - 1, 0)))
    else if (!pageNormal) Cat(ppn(ppnLen - vpnnLen - 1, vpnnLen),
      Mux(inner_level(0), vpn(vpnnLen * 2 - 1, vpnnLen), ppn(vpnnLen - 1,0)),
      vpn(vpnnLen - 1, 0))
    else Cat(ppn(sectorppnLen - 1, vpnnLen * 2 - sectortlbwidth),
      Mux(inner_level(1), vpn(vpnnLen * 2 - 1, vpnnLen), ppn(vpnnLen * 2 - sectortlbwidth - 1, vpnnLen - sectortlbwidth)),
      Mux(inner_level(0), vpn(vpnnLen - 1, 0), Cat(ppn(vpnnLen - sectortlbwidth - 1, 0), ppn_low(vpn(sectortlbwidth - 1, 0)))))

    if (saveLevel) {
      if (ppn.getWidth == ppnLen - vpnnLen) {
        Cat(ppn(ppn.getWidth - 1, vpnnLen * 2), RegEnable(ppn_res(vpnnLen * 2 - 1, 0), valid))
      } else {
        require(ppn.getWidth == sectorppnLen)
        Cat(ppn(ppn.getWidth - 1, vpnnLen * 2 - sectortlbwidth), RegEnable(ppn_res(vpnnLen * 2 - 1, 0), valid))
      }
    }
    else ppn_res
  }

  override def toPrintable: Printable = {
    val inner_level = level.getOrElse(2.U)
    p"asid: ${asid} level:${inner_level} vpn:${Hexadecimal(tag)} ppn:${Hexadecimal(ppn)} perm:${perm}"
  }

}
