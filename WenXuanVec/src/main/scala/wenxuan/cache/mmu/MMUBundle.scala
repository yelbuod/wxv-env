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

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import wenxuan.common._
import wenxuan.backend.rob.RobPtr

abstract class TlbBundle(implicit p: Parameters) extends WXBundle with HasTlbConst
abstract class TlbModule(implicit p: Parameters) extends WXModule with HasTlbConst

/** backend Mem block */
class MemBlockidxBundle(implicit p: Parameters) extends TlbBundle
{

}

/** TLB Bundle */
class TlbReq(implicit p: Parameters) extends TlbBundle {
  val vaddr = Output(UInt(VAddrBits.W))
  val cmd = Output(TlbCmd())
  val size = Output(UInt(log2Ceil(log2Ceil(XLEN/8)+1).W))
  val kill = Output(Bool()) // Use for blocked tlb that need sync with other module like icache
  val memidx = Output(new MemBlockidxBundle)
  // do not translate, but still do pmp/pma check
  val no_translate = Output(Bool())
  // just for debug
  val debug = new Bundle {
    val pc = Output(UInt(XLEN.W))
    val robIdx = Output(new RobPtr)
    val isFirstIssue = Output(Bool())
  }

  // Maybe Block req needs a kill: for itlb, itlb and icache may not sync, itlb should wait icache to go ahead
  override def toPrintable: Printable = {
    p"vaddr:0x${Hexadecimal(vaddr)} cmd:${cmd} kill:${kill} pc:0x${Hexadecimal(debug.pc)} robIdx:${debug.robIdx}"
  }
}

class TlbExceptionBundle(implicit p: Parameters) extends TlbBundle
{
  val ld = Output(Bool())
  val st = Output(Bool())
  val instr = Output(Bool())
}

class TlbResp(nDups: Int = 1)(implicit p: Parameters) extends TlbBundle {
  val paddr = Vec(nDups, Output(UInt(PAddrBits.W)))
  val miss = Output(Bool())
  val excp = Vec(nDups, new Bundle {
    val pf = new TlbExceptionBundle()
    val af = new TlbExceptionBundle()
  })
  val ptwBack = Output(Bool()) // when ptw back, wake up replay rs's state
  val memidx = Output(new MemBlockidxBundle)
  // just for debug
  val debug = new Bundle {
    val robIdx = Output(new RobPtr)
    val isFirstIssue = Output(Bool())
  }
  override def toPrintable: Printable = {
    p"paddr:0x${Hexadecimal(paddr(0))} miss:${miss} excp.pf: ld:${excp(0).pf.ld} st:${excp(0).pf.st} instr:${excp(0).pf.instr} ptwBack:${ptwBack}"
  }
}

class TlbRequestIO(nRespDups: Int = 1)(implicit p: Parameters) extends TlbBundle {
  val req = DecoupledIO(new TlbReq)
  val req_kill = Output(Bool())
  val resp = Flipped(DecoupledIO(new TlbResp(nRespDups)))
}

/** Base Bundle needed by the MMU Module, like TLB, L2TLB, PTW and so on */
class MMUIOBaseBundle(implicit p: Parameters) extends TlbBundle{
  val sfence = Input(new SfenceBundle) // sfence signal sync all MMU module
  val csr = Input(new TlbCsrBundle) // csr operate on MMU module conveniently

  def base_connect(sfence: SfenceBundle, csr: TlbCsrBundle): Unit = {
    this.sfence := sfence
    this.csr := csr
  }

  // overwrite satp. write satp will cause flushpipe but csr.priv won't
  // satp will be dealyed several cycles from writing, but csr.priv won't
  // so inside mmu, these two signals should be divided
  def base_connect(sfence: SfenceBundle, csr: TlbCsrBundle, satp: TlbSatpBundle) = {
    this.sfence <> sfence
    this.csr <> csr
    this.csr.satp := satp
  }
}

class TlbSectorPermBundle(implicit p: Parameters) extends TlbBundle {
  val pf = Bool() // NOTE: if this is true, just raise pf
  val af = Bool() // NOTE: if this is true, just raise af
  // pagetable perm (software defined)
  val d = Bool()
  val a = Bool()
  val g = Bool()
  val u = Bool()
  val x = Bool()
  val w = Bool()
  val r = Bool()

  def apply(item: PtwSectorResp) = {
    val ptePerm = item.entry.perm.get.asTypeOf(new PtePermBundle().cloneType)
    this.pf := item.pf
    this.af := item.af
    this.d := ptePerm.d
    this.a := ptePerm.a
    this.g := ptePerm.g
    this.u := ptePerm.u
    this.x := ptePerm.x
    this.w := ptePerm.w
    this.r := ptePerm.r

    this
  }
  override def toPrintable: Printable = {
    p"pf:${pf} af:${af} d:${d} a:${a} g:${g} u:${u} x:${x} w:${w} r:${r} "
  }
}

class TlbPermBundle(implicit p: Parameters) extends TlbSectorPermBundle {
  def :=(perm: TlbSectorPermBundle) = {
    this.pf := perm.pf
    this.af := perm.af
    this.d  := perm.d
    this.a  := perm.a
    this.g  := perm.g
    this.u  := perm.u
    this.x  := perm.x
    this.w  := perm.w
    this.r  := perm.r

    this
  }
}

class TlbPtwIOwithMemIdx(Width: Int = 1)(implicit p: Parameters) extends TlbBundle {
  val req = Vec(Width, DecoupledIO(new PtwReqwithMemIdx))
  val resp = Flipped(DecoupledIO(new PtwSectorRespwithMemIdx))

  override def toPrintable: Printable = {
    p"req(0):${req(0).valid} ${req(0).ready} ${req(0).bits} | resp:${resp.valid} ${resp.ready} ${resp.bits}"
  }
}

/** PTW issues */
abstract class PtwBundle(implicit p: Parameters) extends WXBundle with HasPtwConst

class PtePermBundle(implicit p: Parameters) extends TlbBundle {
  val d = Bool()
  val a = Bool()
  val g = Bool()
  val u = Bool()
  val x = Bool()
  val w = Bool()
  val r = Bool()

  override def toPrintable: Printable = {
    p"d:${d} a:${a} g:${g} u:${u} x:${x} w:${w} r:${r}"// +
    //(if(hasV) (p"v:${v}") else p"")
  }
}


class PteBundle(implicit p: Parameters) extends PtwBundle{
  val reserved  = UInt(pteResLen.W)
  val ppn_high = UInt(ppnHignLen.W)
  val ppn  = UInt(ppnLen.W)
  val rsw  = UInt(2.W)
  val perm = new Bundle {
    val d    = Bool()
    val a    = Bool()
    val g    = Bool()
    val u    = Bool()
    val x    = Bool()
    val w    = Bool()
    val r    = Bool()
    val v    = Bool()
  }

  def unaligned(level: UInt) = {
    isLeaf() && !(level === 2.U ||
      level === 1.U && ppn(vpnnLen-1,   0) === 0.U ||
      level === 0.U && ppn(vpnnLen*2-1, 0) === 0.U)
  }

  def isPf(level: UInt) = {
    !perm.v || (!perm.r && perm.w) || unaligned(level)
  }

  // paddr of Xiangshan is 36 bits but ppn of sv39 is 44 bits
  // access fault will be raised when ppn >> ppnLen is not zero
  def isAf() = {
    !(ppn_high === 0.U)
  }

  def isLeaf() = {
    perm.r || perm.x || perm.w
  }

  def getPerm() = {
    val pm = Wire(new PtePermBundle)
    pm.d := perm.d
    pm.a := perm.a
    pm.g := perm.g
    pm.u := perm.u
    pm.x := perm.x
    pm.w := perm.w
    pm.r := perm.r
    pm
  }

  override def toPrintable: Printable = {
    p"ppn:0x${Hexadecimal(ppn)} perm:b${Binary(perm.asUInt)}"
  }
}

class PtwEntry(tagLen: Int, hasPerm: Boolean = false, hasLevel: Boolean = false)(implicit p: Parameters) extends PtwBundle {
  val tag = UInt(tagLen.W)
  val asid = UInt(AsidLen.W)
  val ppn = UInt(ppnLen.W)
  val perm = if (hasPerm) Some(new PtePermBundle) else None
  val level = if (hasLevel) Some(UInt(log2Up(Level).W)) else None
  val prefetch = Bool()
  val v = Bool()

  def is_normalentry(): Bool = {
    if (!hasLevel) true.B
    else level.get === 2.U
  }

  def genPPN(vpn: UInt): UInt = {
    if (!hasLevel) ppn
    else MuxLookup(level.get, 0.U, Seq(
      0.U -> Cat(ppn(ppn.getWidth-1, vpnnLen*2), vpn(vpnnLen*2-1, 0)),
      1.U -> Cat(ppn(ppn.getWidth-1, vpnnLen), vpn(vpnnLen-1, 0)),
      2.U -> ppn)
    )
  }

  def hit(vpn: UInt, asid: UInt, allType: Boolean = false, ignoreAsid: Boolean = false) = {
    require(vpn.getWidth == vpnLen)
    //    require(this.asid.getWidth <= asid.getWidth)
    val asid_hit = if (ignoreAsid) true.B else (this.asid === asid)
    if (allType) {
      require(hasLevel)
      val hit0 = tag(tagLen - 1,    vpnnLen*2) === vpn(tagLen - 1, vpnnLen*2)
      val hit1 = tag(vpnnLen*2 - 1, vpnnLen)   === vpn(vpnnLen*2 - 1,  vpnnLen)
      val hit2 = tag(vpnnLen - 1,     0)         === vpn(vpnnLen - 1, 0)

      asid_hit && Mux(level.getOrElse(0.U) === 2.U, hit2 && hit1 && hit0, Mux(level.getOrElse(0.U) === 1.U, hit1 && hit0, hit0))
    } else if (hasLevel) {
      val hit0 = tag(tagLen - 1, tagLen - vpnnLen) === vpn(vpnLen - 1, vpnLen - vpnnLen)
      val hit1 = tag(tagLen - vpnnLen - 1, tagLen - vpnnLen * 2) === vpn(vpnLen - vpnnLen - 1, vpnLen - vpnnLen * 2)

      asid_hit && Mux(level.getOrElse(0.U) === 0.U, hit0, hit0 && hit1)
    } else {
      asid_hit && tag === vpn(vpnLen - 1, vpnLen - tagLen)
    }
  }

  def refill(vpn: UInt, asid: UInt, pte: UInt, level: UInt = 0.U, prefetch: Bool, valid: Bool = false.B) {
    require(this.asid.getWidth <= asid.getWidth) // maybe equal is better, but ugly outside

    tag := vpn(vpnLen - 1, vpnLen - tagLen)
    ppn := pte.asTypeOf(new PteBundle().cloneType).ppn
    perm.map(_ := pte.asTypeOf(new PteBundle().cloneType).perm)
    this.asid := asid
    this.prefetch := prefetch
    this.v := valid
    this.level.map(_ := level)
  }

  def genPtwEntry(vpn: UInt, asid: UInt, pte: UInt, level: UInt = 0.U, prefetch: Bool, valid: Bool = false.B) = {
    val e = Wire(new PtwEntry(tagLen, hasPerm, hasLevel))
    e.refill(vpn, asid, pte, level, prefetch, valid)
    e
  }



  override def toPrintable: Printable = {
    // p"tag:0x${Hexadecimal(tag)} ppn:0x${Hexadecimal(ppn)} perm:${perm}"
    p"tag:0x${Hexadecimal(tag)} ppn:0x${Hexadecimal(ppn)} " +
      (if (hasPerm) p"perm:${perm.getOrElse(0.U.asTypeOf(new PtePermBundle))} " else p"") +
      (if (hasLevel) p"level:${level.getOrElse(0.U)}" else p"") +
      p"prefetch:${prefetch}"
  }
}

class PtwSectorEntry(tagLen: Int, hasPerm: Boolean = false, hasLevel: Boolean = false)(implicit p: Parameters) extends PtwEntry(tagLen, hasPerm, hasLevel) {
  override val ppn = UInt(sectorppnLen.W)
}
class PtwSectorResp(implicit p: Parameters) extends PtwBundle {
  val entry = new PtwSectorEntry(tagLen = sectorvpnLen, hasPerm = true, hasLevel = true)
  val addr_low = UInt(sectortlbwidth.W)
  val ppn_low = Vec(tlbcontiguous, UInt(sectortlbwidth.W))
  val valididx = Vec(tlbcontiguous, Bool())
  val pteidx = Vec(tlbcontiguous, Bool())
  val pf = Bool()
  val af = Bool()

  def genPPN(vpn: UInt): UInt = {
    MuxLookup(entry.level.get, 0.U, Seq(
      0.U -> Cat(entry.ppn(entry.ppn.getWidth-1, vpnnLen * 2 - sectortlbwidth), vpn(vpnnLen*2-1, 0)),
      1.U -> Cat(entry.ppn(entry.ppn.getWidth-1, vpnnLen - sectortlbwidth), vpn(vpnnLen-1, 0)),
      2.U -> Cat(entry.ppn(entry.ppn.getWidth-1, 0), ppn_low(vpn(sectortlbwidth - 1, 0))))
    )
  }

  // determine whether the request is hit the PTW response
  def hit(vpn: UInt, asid: UInt, allType: Boolean = false, ignoreAsid: Boolean = false) = {
    require(vpn.getWidth == vpnLen)
    //    require(this.asid.getWidth <= asid.getWidth)
    val asid_hit = if (ignoreAsid) true.B else (this.entry.asid === asid)
    if (allType) {
      val hit0 = entry.tag(sectorvpnLen - 1, vpnnLen * 2 - sectortlbwidth) === vpn(vpnLen - 1, vpnnLen * 2)
      val hit1 = entry.tag(vpnnLen * 2 - sectortlbwidth - 1, vpnnLen - sectortlbwidth)   === vpn(vpnnLen * 2 - 1,  vpnnLen)
      val hit2 = entry.tag(vpnnLen - sectortlbwidth - 1, 0) === vpn(vpnnLen - 1, sectortlbwidth)
      val addr_low_hit = valididx(vpn(sectortlbwidth - 1, 0))

      asid_hit && Mux(entry.level.getOrElse(0.U) === 2.U, hit2 && hit1 && hit0, Mux(entry.level.getOrElse(0.U) === 1.U, hit1 && hit0, hit0)) && addr_low_hit
    } else {
      val hit0 = entry.tag(sectorvpnLen - 1, sectorvpnLen - vpnnLen) === vpn(vpnLen - 1, vpnLen - vpnnLen)
      val hit1 = entry.tag(sectorvpnLen - vpnnLen - 1, sectorvpnLen - vpnnLen * 2) === vpn(vpnLen - vpnnLen - 1, vpnLen - vpnnLen * 2)
      val addr_low_hit = valididx(vpn(sectortlbwidth - 1, 0))

      asid_hit && Mux(entry.level.getOrElse(0.U) === 0.U, hit0, hit0 && hit1) && addr_low_hit
    }
  }
}

class PtwReq(implicit p: Parameters) extends PtwBundle {
  val vpn = UInt(vpnLen.W)

  override def toPrintable: Printable = {
    p"vpn:0x${Hexadecimal(vpn)}"
  }
}


class PtwResp(implicit p: Parameters) extends PtwBundle {
  val entry = new PtwEntry(tagLen = vpnLen, hasPerm = true, hasLevel = true)
  val pf = Bool()
  val af = Bool()

  def apply(pf: Bool, af: Bool, level: UInt, pte: PteBundle, vpn: UInt, asid: UInt) = {
    this.entry.level.map(_ := level)
    this.entry.tag := vpn
    this.entry.perm.map(_ := pte.getPerm())
    this.entry.ppn := pte.ppn
    this.entry.prefetch := DontCare
    this.entry.asid := asid
    this.entry.v := !pf
    this.pf := pf
    this.af := af
  }

  override def toPrintable: Printable = {
    p"entry:${entry} pf:${pf} af:${af}"
  }
}

class PtwReqwithMemIdx(implicit p: Parameters) extends PtwReq {
  val memidx = new MemBlockidxBundle
}
class PtwSectorRespwithMemIdx(implicit p: Parameters) extends PtwSectorResp {
  val memidx = new MemBlockidxBundle
}

object TlbCmd {
  def read = "b00".U
  def write = "b01".U
  def exec = "b10".U

  def atom_read = "b100".U // lr

  def atom_write = "b101".U // sc / amo
  def apply() = UInt(3.W)


  def isRead(a: UInt) = a(1, 0) === read

  def isWrite(a: UInt) = a(1, 0) === write

  def isExec(a: UInt) = a(1, 0) === exec

  def isAtom(a: UInt) = a(2)

  def isAmo(a: UInt) = a === atom_write // NOTE: sc mixed
}
