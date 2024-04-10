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
import utils._
import utility._

class TlbReplaceAccessBundle(nWays: Int)(implicit p: Parameters) extends TlbBundle {
  val touch_ways = ValidIO(UInt(log2Up(nWays).W))
}

class TlbStorageIO(nWays: Int, ports: Int, nDups: Int = 1)(implicit p: Parameters)
  extends MMUIOBaseBundle
{
  val r = new Bundle {
    val req = Vec(ports, Flipped(DecoupledIO(new Bundle {
      val vpn = Output(UInt(vpnLen.W))
    })))
    val resp = Vec(ports, ValidIO(new Bundle{
      val hit = Output(Bool())
      val ppn = Vec(nDups, Output(UInt(ppnLen.W)))
      val perm = Vec(nDups, Output(new TlbSectorPermBundle()))
    }))
  }
  val w = Flipped(ValidIO(new Bundle {
    val wayIdx = Output(UInt(log2Up(nWays).W))
    val data = Output(new PtwSectorResp)
  }))
  val access = Vec(ports, new TlbReplaceAccessBundle(nWays))

  def r_req_apply(valid: Bool, vpn: UInt, i: Int): Unit = {
    this.r.req(i).valid := valid
    this.r.req(i).bits.vpn := vpn
  }

  def r_resp_apply(i: Int) = {
    (this.r.resp(i).bits.hit, this.r.resp(i).bits.ppn, this.r.resp(i).bits.perm)
  }

  def w_apply(valid: Bool, wayIdx: UInt, data: PtwSectorResp): Unit = {
    this.w.valid := valid
    this.w.bits.wayIdx := wayIdx
    this.w.bits.data := data
  }

}

class TLBFA(
             parentName: String,
             ports: Int,
             nDups: Int,
             nWays: Int,
             saveLevel: Boolean = false,
             normalPage: Boolean,
             superPage: Boolean
           )(implicit p: Parameters) extends TlbModule with HasPerfEvents {

  val io = IO(new TlbStorageIO(nWays, ports, nDups))
  io.r.req.map(_.ready := true.B)

  val valid_way = RegInit(VecInit(Seq.fill(nWays)(false.B)))
  val entries = Reg(Vec(nWays, new TlbSectorEntry(normalPage, superPage)))
  val g = entries.map(_.perm.g)

  // ports combination logic
  for (i <- 0 until ports) {
    /** compares the input request with entries */
    val req = io.r.req(i)
    val resp = io.r.resp(i)
    val access = io.access(i)

    val vpn = req.bits.vpn
    val vpn_reg = RegEnable(vpn, req.fire)

    val refill_mask = Mux(io.w.valid, UIntToOH(io.w.bits.wayIdx), 0.U(nWays.W))
    val hitVec = VecInit(entries.zip(valid_way zip refill_mask.asBools).map{
      case (e, v) => { // exclude the way to be written
        val valid_way_exclude_writing = v._1 && !v._2
        e.hit(vpn, io.csr.satp.asid) && valid_way_exclude_writing
      }
    })

    hitVec.suggestName("hitVec")

    /** pipe2: drive the response */
    val hitVecReg = RegEnable(hitVec, req.fire)
    // Sector tlb may trigger multi-hit, see def "wbhit"
    XSPerfAccumulate(s"port${i}_multi_hit", !(!resp.valid || (PopCount(hitVecReg) === 0.U || PopCount(hitVecReg) === 1.U)))

    resp.valid := RegNext(req.valid)
    resp.bits.hit := Cat(hitVecReg).orR
    if (nWays == 1) {
      for (d <- 0 until nDups) {
        resp.bits.ppn(d) := RegEnable(entries(0).genPPN(saveLevel, req.valid)(vpn), req.fire)
        resp.bits.perm(d) := RegEnable(entries(0).perm, req.fire)
      }
    } else {
      for (d <- 0 until nDups) {
        resp.bits.ppn(d) := RegEnable(ParallelMux(hitVec zip entries.map(_.genPPN(saveLevel, req.valid)(vpn))), req.fire)
        resp.bits.perm(d) := RegEnable(ParallelMux(hitVec zip entries.map(_.perm)), req.fire)
      }
    }

      /** record the number of way hit by request for replacer update */
    access.touch_ways.valid := resp.valid && Cat(hitVecReg).orR
    access.touch_ways.bits := OHToUInt(hitVecReg)

    resp.bits.hit.suggestName("hit")
    resp.bits.ppn.suggestName("ppn")
    resp.bits.perm.suggestName("perm")
  }

  when (io.w.valid) {
    valid_way(io.w.bits.wayIdx) := true.B
    entries(io.w.bits.wayIdx).apply(io.w.bits.data, io.csr.satp.asid)
  }
  // write assert, should not duplicate with the existing entries
  val w_hit_vec = VecInit(entries.zip(valid_way).map{case (e, vi) => e.wbhit(io.w.bits.data, io.csr.satp.asid) && vi })
  XSError(io.w.valid && Cat(w_hit_vec).orR, s"${parentName} refill, duplicate with existing entries")

  /** pipe2: record write information to touch_ways for replacer update */
  val refill_vpn_reg = RegNext(io.w.bits.data.entry.tag)
  val refill_wayIdx_reg = RegNext(io.w.bits.wayIdx)
  when (RegNext(io.w.valid)) { /** all ports of access are connect by write info when write valid */
    io.access.map { access =>
      access.touch_ways.valid := true.B
      access.touch_ways.bits := refill_wayIdx_reg
    }
  }

  /** sfence flush */
  val sfence = io.sfence
  val sfence_vpn = get_vpn(sfence.bits.addr)
  val sfenceHit = entries.map(_.hit(sfence_vpn, sfence.bits.asid))
  val sfenceHit_noasid = entries.map(_.hit(sfence_vpn, sfence.bits.asid, ignoreAsid = true))
  // Sfence will flush all sectors of an entry when hit
  when (io.sfence.valid) {
    when (sfence.bits.rs1) { // virtual address *.rs1 <- (rs1===0.U)
      when (sfence.bits.rs2) { // asid, but i do not want to support asid, *.rs2 <- (rs2===0.U)
        // all addr and all asid
        valid_way.map(_ := false.B)
      }.otherwise {
        // all addr but specific asid
        valid_way.zipWithIndex.map{ case (a,i) => a := a & (g(i) | !(entries(i).asid === sfence.bits.asid)) }
      }
    }.otherwise {
      when (sfence.bits.rs2) {
        // specific addr but all asid
        valid_way.zipWithIndex.map{ case (a,i) => a := a & !sfenceHit_noasid(i) }
      }.otherwise {
        // specific addr and specific asid
        valid_way.zipWithIndex.map{ case (a,i) => a := a & !(sfenceHit(i) && !g(i)) }
      }
    }
  }

  XSPerfAccumulate(s"access", io.r.resp.map(_.valid.asUInt).fold(0.U)(_ + _))
  XSPerfAccumulate(s"hit", io.r.resp.map(a => a.valid && a.bits.hit).fold(0.U)(_.asUInt + _.asUInt))

  for (i <- 0 until nWays) {
    XSPerfAccumulate(s"access${i}", io.r.resp.zip(io.access.map(acc => UIntToOH(acc.touch_ways.bits))).map{ case (a, b) =>
      a.valid && a.bits.hit && b(i)}.fold(0.U)(_.asUInt + _.asUInt))
  }
  for (i <- 0 until nWays) {
    XSPerfAccumulate(s"refill${i}", io.w.valid && io.w.bits.wayIdx === i.U)
  }

  val perfEvents = Seq(
    ("tlbstore_access", io.r.resp.map(_.valid.asUInt).fold(0.U)(_ + _)                            ),
    ("tlbstore_hit   ", io.r.resp.map(a => a.valid && a.bits.hit).fold(0.U)(_.asUInt + _.asUInt)),
  )
  generatePerfEvent()

  println(s"${parentName} tlb_fa: nWays:${nWays}")
}

object TlbStorage {
  def apply
  (
    parentName: String,
    ports: Int,
    nDups: Int = 1,
    nSets: Int,
    nWays: Int,
    saveLevel: Boolean = false,
    normalPage: Boolean,
    superPage: Boolean,
    useDmode: Boolean,
  )(implicit p: Parameters) = {
    val storage = Module(new TLBFA(parentName, ports, nDups, nWays, saveLevel, normalPage, superPage))
    storage.suggestName(s"${parentName}_fa")
    storage.io
  }
}


class ReplaceIO(ports: Int, nSets: Int, nWays: Int)(implicit p: Parameters) extends TlbBundle {
  val access = Vec(ports, new TlbReplaceAccessBundle(nWays)) // output to tell the touch ways info to replacer
  val refillIdx = Input(UInt(log2Up(nWays).W)) // input to define which way to be replaced to write a new one
  val chosen_set = Output(UInt(log2Up(nSets).W))

  def apply_sep(in: Seq[ReplaceIO], vpn: UInt): Unit = {
    for ((ac_rep, ac_tlb) <- access.zip(in.map(i => i.access.map(ac_porti => ac_porti)).flatten)) {
      ac_rep := ac_tlb
    }
    this.chosen_set := get_set_idx(vpn, nSets)
    in.map(a => a.refillIdx := this.refillIdx)
  }
}

class TlbReplaceIO(ports: Int, q: TLBParams)(implicit p: Parameters) extends
  TlbBundle {
  val page = new ReplaceIO(ports, q.nSets, q.nWays)

  def apply_sep(in: Seq[TlbReplaceIO], vpn: UInt) = {
    this.page.apply_sep(in.map(_.page), vpn)
  }

}

class TlbStorageWrapperIO(ports: Int, q: TLBParams, nDups: Int = 1)(implicit p: Parameters)
  extends MMUIOBaseBundle
{
  val r = new Bundle {
    val req = Vec(ports, Flipped(DecoupledIO(new Bundle {
      val vpn = Output(UInt(vpnLen.W))
    })))
    val resp = Vec(ports, ValidIO(new Bundle{
      val hit = Output(Bool())
      val ppn = Vec(nDups, Output(UInt(ppnLen.W)))
      val perm = Vec(nDups, Output(new TlbPermBundle()))
    }))
  }
  val w = Flipped(ValidIO(new Bundle {
    val data = Output(new PtwSectorResp)
  }))
  val replace = if (q.outReplace) new TlbReplaceIO(ports, q) else null

  def r_req_apply(valid: Bool, vpn: UInt, i: Int): Unit = {
    this.r.req(i).valid := valid
    this.r.req(i).bits.vpn := vpn
  }

  def r_resp_apply(i: Int) = {
    (this.r.resp(i).bits.hit, this.r.resp(i).bits.ppn, this.r.resp(i).bits.perm)
  }

  def w_apply(valid: Bool, data: PtwSectorResp): Unit = {
    this.w.valid := valid
    this.w.bits.data := data
  }
}

class TlbStorageWrapper(ports: Int, q: TLBParams, nDups: Int = 1)(implicit p: Parameters)
  extends TlbModule
{
  val io = IO(new TlbStorageWrapperIO(ports, q, nDups))

  val page = TlbStorage(
    parentName = q.name + "_storage",
    ports = ports,
    nDups = nDups,
    nSets = q.nSets,
    nWays = q.nWays,
    normalPage = true,
    superPage = true,
    useDmode = q.useDmode,
  )

  for (i <- 0 until ports) {
    io.r.req(i) <> page.r.req(i)
  }

  for (i <- 0 until ports) {
    val from_page = page.r.resp(i)
    val io_resp = io.r.resp(i)
    io_resp.valid := from_page.valid // actually, not used
    io_resp.bits.hit := from_page.bits.hit
    for (d <- 0 until nDups) {
      io_resp.bits.ppn(d) := from_page.bits.ppn(d)
      io_resp.bits.perm(d) := from_page.bits.perm(d)
    }
  }

  page.sfence <> io.sfence
  page.csr <> io.csr

  /** wrapper replacer or outReplace logic */
  val refill_idx = if (q.outReplace) {
    io.replace.page.access <> page.access
    io.replace.page.chosen_set := DontCare
    io.replace.page.refillIdx
  } else {
    val re = ReplacementPolicy.fromString(q.Replacer, q.nWays)
    re.access(page.access.map(_.touch_ways))
    re.way
  }

  page.w_apply(
    valid = io.w.valid,
    wayIdx = refill_idx,
    data = io.w.bits.data
  )

  // replacement
  def get_access(one_hot: UInt, valid: Bool): Valid[UInt] = {
    val res = Wire(Valid(UInt(log2Up(one_hot.getWidth).W)))
    res.valid := Cat(one_hot).orR && valid
    res.bits := OHToUInt(one_hot)
    res
  }
}

