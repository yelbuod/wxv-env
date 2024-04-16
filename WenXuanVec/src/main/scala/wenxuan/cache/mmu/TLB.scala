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
import difftest.{DiffL1TLBEvent, DifftestModule}
import org.chipsalliance.cde.config.Parameters
import wenxuan.backend.fu.PMPReqBundle
import wenxuan.backend.fu.HasCSRConst
import utility._
import utils._
import wxv_util._

class TLBIO(PortNum: Int, nRespDups: Int = 1, q: TLBParams)(implicit p: Parameters)
  extends MMUIOBaseBundle
{
  val hartId = Input(UInt(8.W))
  val flushPipe = Vec(PortNum, Input(Bool()))
  val requestor = Vec(PortNum, Flipped(new TlbRequestIO(nRespDups)))
  val ptw = new TlbPtwIOwithMemIdx(PortNum)
  val replace = if (q.outReplace) Flipped(new TlbReplaceIO(PortNum, q)) else null
  val pmp = Vec(PortNum, ValidIO(new PMPReqBundle()))
  val tlbreplay = Vec(PortNum, Output(Bool()))
}

/** TLB Module
 * support block and non-block request io at the same time
 * return paddr at next cycle
 * @param PortNum: the number of requestors
 * @param nRespDups: the duplicate of response
 * @param Block: Blocked or non-block attributes of each request port
 * @param q: TLB Parameters, like entry number, each TLB has its own params
 */
class TLB(PortNum: Int, nRespDups: Int = 1, Block: Seq[Boolean], q: TLBParams)(implicit p: Parameters)
  extends TlbModule with TLBMethod(PortNum, nRespDups, q) with HasCSRConst with HasPerfEvents
{
  val io  = IO(new TLBIO(PortNum, nRespDups, q))

  val req = io.requestor.map(_.req) // Vec(PortNum)
  val resp = io.requestor.map(_.resp) // Vec(PortNum, Vec(nRespDups))
  val (toPTW, fromPTW) = (io.ptw.req, io.ptw.resp) // req: Vec(PortNum), resp: PtwSectorRespwithMemIdx
  val toPMP = io.pmp

  fromPTW.ready := true.B

  /** Sfence.vma & Svinval
   * Sfence.vma will 1. flush old tlb_entries 2. flush inflight 3. flush pipe
   * Svinval will 1. flush old tlb_entries 2. flush inflight
   * So, Svinval will not flush pipe, which means
   * it should not drop reqs from pipe and should return right resp
   */
  val sfence = DelayN(io.sfence, q.fenceDelay) // flush old entries
  val csr = io.csr
  val satp = DelayN(io.csr.satp, q.fenceDelay)
  val flush_mmu = DelayN(sfence.valid || csr.satp.changed, q.fenceDelay)
  val mmu_flush_pipe = DelayN(sfence.valid && sfence.bits.flushPipe, q.fenceDelay) // for svinval, won't flush pipe
  val flush_pipe = io.flushPipe // from backend redirect

  val ifecth = if (q.ifecth) true.B else false.B

  val mode = if (q.useDmode) csr.priv.dmode else csr.priv.imode // privilege level
  val vmEnable = if (EnbaleTlbDebug) (satp.mode === 8.U)
                 else (satp.mode === 8.U && (mode < ModeM))
  val portTranslateEnable = (0 until PortNum).map(
    i => vmEnable && RegNext(!req(i).bits.no_translate)
  )

  val tlb_entries = Module(new TlbStorageWrapper(PortNum, q, nRespDups))
  tlb_entries.io.base_connect(sfence, csr, satp)
  if (q.outReplace) { io.replace <> tlb_entries.io.replace }

  // requestor -> TLB
  for (i <- 0 until PortNum) {
    tlb_entries.io.r_req_apply(req(i).fire, req(i).bits.vaddr, i)
    resp(i).bits.debug.isFirstIssue := RegNext(req(i).bits.debug.isFirstIssue) // just for debug
    resp(i).bits.debug.robIdx := RegNext(req(i).bits.debug.robIdx)
  }
  // PTW refill -> TLB
  val refill_valid = fromPTW.fire && vmEnable && !flush_mmu
  tlb_entries.io.w_apply(refill_valid, fromPTW.bits)

  // read TLB, get hit/miss, paddr, perm bits
  val readResult = (0 until PortNum).map(GetTLBResp(_))
  val hitVec = readResult.map(_._1)
  val missVec = readResult.map(_._2)
  val pmp_addr = readResult.map(_._3)
  val perm = readResult.map(_._4)

  // check pmp use paddr (for timing optization, use pmp_addr here)
  // check permisson
  (0 until PortNum).foreach { i =>
    pmp_check(pmp_addr(i), req_out(i).size, req_out(i).cmd, i) // output to check pmp but no resp?
    for (d <- 0 until nRespDups) {
      perm_check(perm(i)(d), req_out(i).cmd, i, d)
    }
  }

  /** handle block or non-block io */
  // for non-block io, just return the above result, send miss to ptw
  // for block io, hold the request, send miss to ptw, when ptw back, return the result
  (0 until PortNum) foreach { i =>
    if (Block(i)) handle_block(i)
    else handle_nonblock(i)
  }


  // assert
  for (i <- 0 until PortNum) {
    TimeOutAssert(req_out_v(i) && !resp(i).valid, timeOutThreshold, s"{q.name} port{i} long time no resp valid.")
  }

  /** perf event */
  val result_ok = req_in.map(a => RegNext(a.fire))
  val perfEvents =
    Seq(
      ("access", PopCount((0 until PortNum).map { i => if (Block(i)) io.requestor(i).req.fire else portTranslateEnable(i) && result_ok(i) })),
      ("miss  ", PopCount((0 until PortNum).map { i => if (Block(i)) portTranslateEnable(i) && result_ok(i) && missVec(i) else toPTW(i).fire })),
    )
  generatePerfEvent()

  // perf log
  for (i <- 0 until PortNum) {
    if (Block(i)) {
      XSPerfAccumulate(s"access${i}", result_ok(i) && portTranslateEnable(i))
      XSPerfAccumulate(s"miss${i}", result_ok(i) && missVec(i))
    } else {
      XSPerfAccumulate("first_access" + Integer.toString(i, 10), result_ok(i) && portTranslateEnable(i) && RegNext(req(i).bits.debug.isFirstIssue))
      XSPerfAccumulate("access" + Integer.toString(i, 10), result_ok(i) && portTranslateEnable(i))
      XSPerfAccumulate("first_miss" + Integer.toString(i, 10), result_ok(i) && portTranslateEnable(i) && missVec(i) && RegNext(req(i).bits.debug.isFirstIssue))
      XSPerfAccumulate("miss" + Integer.toString(i, 10), result_ok(i) && portTranslateEnable(i) && missVec(i))
    }
  }
  XSPerfAccumulate("ptw_resp_count", fromPTW.fire)
  XSPerfAccumulate("ptw_resp_pf_count", fromPTW.fire && fromPTW.bits.pf)

  // Log
  for (i <- 0 until PortNum) {
    XSDebug(req(i).valid, p"req(${i.U}): (${req(i).valid} ${req(i).ready}) ${req(i).bits}\n")
    XSDebug(resp(i).valid, p"resp(${i.U}): (${resp(i).valid} ${resp(i).ready}) ${resp(i).bits}\n")
  }

  XSDebug(io.sfence.valid, p"Sfence: ${io.sfence}\n")
  XSDebug(ParallelOR(req_out_v) || fromPTW.valid, p"vmEnable:${vmEnable} hit:${Binary(VecInit(hitVec).asUInt)} miss:${Binary(VecInit(missVec).asUInt)}\n")
  for (i <- toPTW.indices) {
    XSDebug(toPTW(i).fire, p"L2TLB req:${toPTW(i).bits}\n")
  }
  XSDebug(fromPTW.valid, p"L2TLB resp:${fromPTW.bits} (v:${fromPTW.valid}r:${fromPTW.ready}) \n")

  println(s"${q.name}: page: ${q.nWays} ${q.Associative} ${q.Replacer.get}")

//  if (env.EnableDifftest) {
//    for (i <- 0 until PortNum) {
//      val pf = io.requestor(i).resp.bits.excp(0).pf.instr || io.requestor(i).resp.bits.excp(0).pf.st || io.requestor(i).resp.bits.excp(0).pf.ld
//      val af = io.requestor(i).resp.bits.excp(0).af.instr || io.requestor(i).resp.bits.excp(0).af.st || io.requestor(i).resp.bits.excp(0).af.ld
//      val difftest = DifftestModule(new DiffL1TLBEvent)
//      difftest.coreid := io.hartId
//      difftest.valid := RegNext(io.requestor(i).req.fire) && !io.requestor(i).req_kill && io.requestor(i).resp.fire && !io.requestor(i).resp.bits.miss && !pf && !af && portTranslateEnable(i)
//      if (!Seq("itlb", "ldtlb", "sttlb").contains(q.name)) {
//        difftest.valid := false.B
//      }
//      difftest.index := TLBDiffId(HartId).U
//      difftest.satp := io.csr.satp.ppn
//      difftest.vpn := RegNext(get_pn(req_in(i).bits.vaddr))
//      difftest.ppn := get_pn(io.requestor(i).resp.bits.paddr(0))
//    }
//  }
}

trait TLBMethod(PortNum: Int, nRespDups: Int, q: TLBParams) {this: TLB =>

  val req_in = req
  val req_out = req.map(a => RegEnable(a.bits, a.fire))
  val req_out_v = (0 until PortNum).map(i => ValidHold(req_in(i).fire && !req_in(i).bits.kill, resp(i).fire, flush_pipe(i)))

  /** return info : hit, miss, paddr, perm bits */
  def GetTLBResp(i: Int) = {
    val (e_hit, e_ppn, e_perm) = tlb_entries.io.r_resp_apply(i)
    val (p_hit, p_ppn, p_perm) = ptw_resp_bypass(get_pn(req_in(i).bits.vaddr))
    val enable = portTranslateEnable(i)

    val hit = e_hit || p_hit
    val miss = !hit && enable
    hit.suggestName(s"hit_read_${i}")
    miss.suggestName(s"miss_read_${i}")

    val vaddr = SignExt(req_out(i).vaddr, PAddrBits)
    resp(i).bits.miss := miss
    resp(i).bits.ptwBack := fromPTW.fire
    resp(i).bits.memidx := RegNext(req_in(i).bits.memidx) // ?

    val ppn = WireInit(VecInit(Seq.fill(nRespDups)(0.U(ppnLen.W))))
    val perm = WireInit(VecInit(Seq.fill(nRespDups)(0.U.asTypeOf(new TlbPermBundle))))

    for (d <- 0 until nRespDups) {
      ppn(d) := Mux(p_hit, p_ppn, e_ppn(d))
      perm(d) := Mux(p_hit, p_perm, e_perm(d))

      val paddr = Cat(ppn(d), get_off(req_out(i).vaddr))
      resp(i).bits.paddr(d) := Mux(enable, paddr, vaddr)
    }

    XSDebug(req_out_v(i), p"(${i.U}) hit:${hit} miss:${miss} ppn:${Hexadecimal(ppn(0))} perm:${perm(0)}\n")

    val pmp_paddr = resp(i).bits.paddr(0)

    (hit, miss, pmp_paddr, perm)
  }

  def pmp_check(paddr: UInt, size: UInt, cmd: UInt, idx: Int): Unit = {
    toPMP(idx).valid := resp(idx).valid
    toPMP(idx).bits.paddr := paddr
    toPMP(idx).bits.size := size
    toPMP(idx).bits.cmd := cmd
  }

  def perm_check(perm: TlbPermBundle, cmd: UInt, idx: Int, nDups: Int) = {
    // for timing optimization, pmp check is divided into dynamic and static
    // dynamic: superpage (or full-connected reg tlb_entries) -> check pmp when translation done
    // static: 4K pages (or sram tlb_entries) -> check pmp with pre-checked results
    val af = perm.af
    val pf = perm.pf
    val ldUpdate = !perm.a && TlbCmd.isRead(cmd) && !TlbCmd.isAmo(cmd) // update A/D through exception
    val stUpdate = (!perm.a || !perm.d) && (TlbCmd.isWrite(cmd) || TlbCmd.isAmo(cmd)) // update A/D through exception
    val instrUpdate = !perm.a && TlbCmd.isExec(cmd) // update A/D through exception
    val modeCheck = !(mode === ModeU && !perm.u || mode === ModeS && perm.u && (!io.csr.priv.sum || ifecth))
    val ldPermFail = !(modeCheck && (perm.r || io.csr.priv.mxr && perm.x))
    val stPermFail = !(modeCheck && perm.w)
    val instrPermFail = !(modeCheck && perm.x)
    val ldPf = (ldPermFail || pf) && (TlbCmd.isRead(cmd) && !TlbCmd.isAmo(cmd))
    val stPf = (stPermFail || pf) && (TlbCmd.isWrite(cmd) || TlbCmd.isAmo(cmd))
    val instrPf = (instrPermFail || pf) && TlbCmd.isExec(cmd)
    val fault_valid = portTranslateEnable(idx)
    resp(idx).bits.excp(nDups).pf.ld := (ldPf || ldUpdate) && fault_valid && !af
    resp(idx).bits.excp(nDups).pf.st := (stPf || stUpdate) && fault_valid && !af
    resp(idx).bits.excp(nDups).pf.instr := (instrPf || instrUpdate) && fault_valid && !af
    // NOTE: pf need && with !af, page fault has higher priority than access fault
    // but ptw may also have access fault, then af happens, the translation is wrong.
    // In this case, pf has lower priority than af

    resp(idx).bits.excp(nDups).af.ld := af && TlbCmd.isRead(cmd) && fault_valid
    resp(idx).bits.excp(nDups).af.st := af && TlbCmd.isWrite(cmd) && fault_valid
    resp(idx).bits.excp(nDups).af.instr := af && TlbCmd.isExec(cmd) && fault_valid
  }

  def handle_nonblock(idx: Int): Unit = {
    io.requestor(idx).resp.valid := req_out_v(idx)
    io.requestor(idx).req.ready := io.requestor(idx).resp.ready // should always be true
    XSError(!io.requestor(idx).resp.ready, s"${q.name} port ${idx} is non-block, resp.ready must be true.B")

    val ptw_just_back = fromPTW.fire && fromPTW.bits.hit(get_pn(req_out(idx).vaddr), asid = io.csr.satp.asid, allType = true)
    val ptw_already_back = RegNext(fromPTW.fire) && RegNext(fromPTW.bits).hit(get_pn(req_out(idx).vaddr), asid = io.csr.satp.asid, allType = true)
    toPTW(idx).valid := req_out_v(idx) && missVec(idx) && !(ptw_just_back || ptw_already_back) // TODO: remove the regnext, timing
    io.tlbreplay(idx) := req_out_v(idx) && missVec(idx) && (ptw_just_back || ptw_already_back)
    when(io.requestor(idx).req_kill && RegNext(io.requestor(idx).req.fire)) {
      toPTW(idx).valid := false.B
      io.tlbreplay(idx) := true.B
    }
    toPTW(idx).bits.vpn := get_pn(req_out(idx).vaddr)
    toPTW(idx).bits.memidx := req_out(idx).memidx
  }


  def handle_block(idx: Int): Unit = {
    // three valid: 1.if exist a entry; 2.if sent to ptw; 3.unset resp.valid
    io.requestor(idx).req.ready := !req_out_v(idx) || io.requestor(idx).resp.fire
    // req_out_v for if there is a request, may long latency, fixme

    // miss request tlb_entries
    val miss_req_vpn = get_pn(req_out(idx).vaddr)
    val miss_req_memidx = req_out(idx).memidx

    // block req keep sending continuously when it miss,
    // so miss_req_v will keep valid to send toPTW, until get PTW resp and hit
    val new_coming = RegNext(req_in(idx).fire && !req_in(idx).bits.kill && !flush_pipe(idx), false.B)
    val miss_wire = new_coming && missVec(idx)
    val miss_v = ValidHoldBypass(miss_wire, resp(idx).fire, flush_pipe(idx))
    val miss_req_v = ValidHoldBypass(miss_wire || (miss_v && !flush_mmu && !mmu_flush_pipe),
      toPTW(idx).fire || resp(idx).fire, flush_pipe(idx))

    // initial response :
    // resp will invalid for request that miss
    // resp always valid for no enable translate req
    // may be changed when PTW resp
    resp(idx).valid := req_out_v(idx) && !(miss_v && portTranslateEnable(idx))

    // send miss request to PTW to refill
    toPTW(idx).valid := miss_req_v
    toPTW(idx).bits.vpn := miss_req_vpn
    toPTW(idx).bits.memidx := miss_req_memidx

    // when ptw resp, check if hit, reset miss_v, resp to lsu/ifu
    val hit = fromPTW.bits.hit(miss_req_vpn, io.csr.satp.asid, allType = true) && fromPTW.valid
    when(fromPTW.fire && hit && req_out_v(idx) && portTranslateEnable(idx)) {
      val pte = fromPTW.bits // get the PTE from PTW resp
      resp(idx).valid := true.B
      resp(idx).bits.miss := false.B // for blocked tlb, this is useless
      for (d <- 0 until nRespDups) {
        resp(idx).bits.paddr(d) := Cat(pte.genPPN(get_pn(req_out(idx).vaddr)), get_off(req_out(idx).vaddr))
        perm_check(pte, req_out(idx).cmd, idx, d)
      }
      pmp_check(resp(idx).bits.paddr(0), req_out(idx).size, req_out(idx).cmd, idx)

      // NOTE: the unfiltered req would be handled by Repeater
    }
    assert(RegNext(!resp(idx).valid || resp(idx).ready, true.B), "when tlb resp valid, ready should be true, must")
    assert(RegNext(req_out_v(idx) || !(miss_v || miss_req_v), true.B), "when not req_out_v, should not set miss_v/miss_req_v")

    io.tlbreplay(idx) := false.B // no use in block req

    // NOTE: when flush pipe, tlb should abandon last req
    // however, some outside modules like icache, dont care flushPipe, and still waiting for tlb resp
    // just resp valid and raise page fault to go through. The pipe(ifu) will abandon it.
    if (!q.outsideRecvFlush) {
      when(req_out_v(idx) && flush_pipe(idx) && portTranslateEnable(idx)) {
        resp(idx).valid := true.B
        for (d <- 0 until nRespDups) {
          resp(idx).bits.excp(d).pf.ld := true.B // sfence happened, pf for not to use this addr
          resp(idx).bits.excp(d).pf.st := true.B
          resp(idx).bits.excp(d).pf.instr := true.B
        }
      }
    }
  }

  // when ptw resp, tlb at refill_idx maybe set to miss by force.
  // Bypass ptw resp to check.
  def ptw_resp_bypass(vpn: UInt) = {
    val p_hit = RegNext(fromPTW.bits.hit(vpn, csr.satp.asid, allType = true) && fromPTW.fire)
    val p_ppn = RegEnable(fromPTW.bits.genPPN(vpn), fromPTW.fire)
    val p_perm = RegEnable(ptwresp_to_tlbperm(fromPTW.bits), fromPTW.fire)
    (p_hit, p_ppn, p_perm)
  }

}

object TLBDiffId {
  var i: Int = 0
  var lastHartId: Int = -1
  def apply(hartId: Int): Int = {
    if (lastHartId != hartId) {
      i = 0
      lastHartId = hartId
    }
    i += 1
    i - 1
  }
}