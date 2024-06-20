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

package wenxuan.backend.rename

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility.ParallelPriorityMux
import utils.XSError
import wenxuan.backend.{RobCommitIO, SnapshotPort}
import wenxuan.common.{WXBundle, WXModule}

class RatReadPort(implicit p: Parameters) extends WXBundle {
  val hold = Input(Bool())
  val addr = Input(UInt(5.W))
  val data = Output(UInt(PhyRegIdxWidth.W))
}

class RatWritePort(implicit p: Parameters) extends WXBundle {
  val wen = Bool()
  val addr = UInt(5.W)
  val data = UInt(PhyRegIdxWidth.W)
}

class RenameTable(float: Boolean)(implicit p: Parameters) extends WXModule {
  val io = IO(new Bundle {
    val redirect = Input(Bool()) // resume table and flush all inflight reqs and snapshot
    val readPorts = Vec({if(float) 4 else 3} * RenameWidth, new RatReadPort)
    val specWritePorts = Vec(CommitWidth, Input(new RatWritePort))
    val archWritePorts = Vec(CommitWidth, Input(new RatWritePort))
    val old_pdest = Vec(CommitWidth, Output(UInt(PhyRegIdxWidth.W)))
    val need_free = Vec(CommitWidth, Output(Bool()))
    val snpt = Input(new SnapshotPort)
    val debug_rdata = Vec(32, Output(UInt(PhyRegIdxWidth.W)))
  })

  val rename_table_init = VecInit.tabulate(32)(i => (if (float) i else 0).U(PhyRegIdxWidth.W))
  // speculative state rename table
  val spec_table = RegInit(rename_table_init)
  val spec_table_next = WireInit(spec_table)
  // arch state rename table
  val arch_table = RegInit(rename_table_init)
  val arch_table_next = WireInit(arch_table)
  // old_pdest to free phy reg to freelist
  val old_pdest = RegInit(VecInit.fill(CommitWidth)(0.U(PhyRegIdxWidth.W)))
  val need_free = RegInit(VecInit.fill(CommitWidth)(false.B))

  // For better timing, we optimize reading and writing to RenameTable as follows:
  // (1) Writing at T0 will be actually processed at T1.
  // (2) Reading is synchronous now.
  // (3) RAddr at T0 will be used to access the table and get data at T0.
  // (4) WData at T0 is bypassed to RData at T1.
  val t1_redirect = RegNext(io.redirect, false.B)
  val t1_rdata = io.readPorts.map(p => RegNext(Mux(p.hold, p.data, spec_table_next(p.addr))))
  val t1_raddr = io.readPorts.map(p => RegEnable(p.addr, !p.hold))
  val t1_wSpec = RegNext(Mux(io.redirect, 0.U.asTypeOf(io.specWritePorts), io.specWritePorts))

  val t1_snpt = RegNext(io.snpt, 0.U.asTypeOf(io.snpt))

  // t1_redirect : redirect will flush snapshot and use snapshot resume table at the same time (t1 stage)
  val snapshots = SnapshotGenerator(spec_table, t1_snpt.snptEnq, t1_snpt.snptDeq, t1_redirect)

  // WRITE: when instruction commits or walking
  val t1_wSpec_addr_oh = t1_wSpec.map(w => Mux(w.wen, UIntToOH(w.addr), 0.U))
  for ((next, i) <- spec_table_next.zipWithIndex) {
    val matchVec = t1_wSpec_addr_oh.map(w => w(i))
    // select newest map to write to rename table
    val wMatch = ParallelPriorityMux(matchVec.reverse, t1_wSpec.map(_.data).reverse)
    // When there's a flush, we use arch_table to update spec_table.
    next := Mux(
      t1_redirect,
      Mux(t1_snpt.useSnpt, snapshots(t1_snpt.snptSelect)(i), arch_table(i)),
      Mux(VecInit(matchVec).asUInt.orR, wMatch, spec_table(i))
    )
  }
  spec_table := spec_table_next

  // READ: decode-rename stage
  for ((r, i) <- io.readPorts.zipWithIndex) {
    // We use two comparisons here because r.hold has bad timing but addrs have better timing.
    val t0_bypass = io.specWritePorts.map(w => w.wen && Mux(r.hold, w.addr === t1_raddr(i), w.addr === r.addr))
    val t1_bypass = RegNext(Mux(io.redirect, 0.U.asTypeOf(VecInit(t0_bypass)), VecInit(t0_bypass)))
    val bypass_data = ParallelPriorityMux(t1_bypass.reverse, t1_wSpec.map(_.data).reverse)
    r.data := Mux(t1_bypass.asUInt.orR, bypass_data, t1_rdata(i))
  }

  // WRITE: update arch state rename table
  for ((w, i) <- io.archWritePorts.zipWithIndex) {
    when(w.wen) {
      arch_table_next(w.addr) := w.data
    }
    val arch_mask = VecInit.fill(PhyRegIdxWidth)(w.wen).asUInt
    old_pdest(i) :=
      // default, arch_mask mask no wen commit which no writeback means no allow to release old pdest
      MuxCase(arch_table(w.addr) & arch_mask,
        // .take(i): ensure get real nearest old pdest in WAW
        io.archWritePorts.take(i).reverse.map(x => (x.wen && x.addr === w.addr, x.data & arch_mask)))
  }
  arch_table := arch_table_next

  for (((old, free), i) <- (old_pdest zip need_free).zipWithIndex) {
    val hasDuplicate = old_pdest.take(i).map(_ === old)
    val blockedByDup = if (i == 0) false.B else VecInit(hasDuplicate).asUInt.orR
    free := VecInit(arch_table.map(_ =/= old)).asUInt.andR && !blockedByDup
  }

  io.old_pdest := old_pdest
  io.need_free := need_free
  io.debug_rdata := arch_table
}

class RenameTableWrapper(implicit p: Parameters) extends WXModule {
  val io = IO(new Bundle{
    val redirect = Input(Bool())
    // ROB commit write arch table or WALK write mapping after redirect
    val robCommits = Input(new RobCommitIO)
    // read reg <> phy reg map
    val intReadPorts = Vec(RenameWidth, Vec(3, new RatReadPort))
    val fpReadPorts = Vec(RenameWidth, Vec(4, new RatReadPort))
    // write speculative new allocated map
    val intRenamePorts = Vec(RenameWidth, Input(new RatWritePort))
    val fpRenamePorts = Vec(RenameWidth, Input(new RatWritePort))
    // old pdest & need free for release to freelist
    val int_old_pdest = Vec(CommitWidth, Output(UInt(PhyRegIdxWidth.W)))
    val fp_old_pdest = Vec(CommitWidth, Output(UInt(PhyRegIdxWidth.W)))
    val int_need_free = Vec(CommitWidth, Output(Bool()))
    val fp_need_free = Vec(CommitWidth, Output(Bool()))
    // snapshot enq/deq and resume from snapshot
    val snpt = Input(new SnapshotPort)
    // debug
    val debug_int_rat = Vec(32, Output(UInt(PhyRegIdxWidth.W)))
    val debug_fp_rat = Vec(32, Output(UInt(PhyRegIdxWidth.W)))
  })

  val intRat = Module(new RenameTable(float = false))
  val fpRat = Module(new RenameTable(float = true))

  intRat.io.redirect := io.redirect
  fpRat.io.redirect := io.redirect
  // rename read logic reg map to phy reg
  intRat.io.readPorts <> io.intReadPorts.flatten
  fpRat.io.readPorts <> io.fpReadPorts.flatten

  // allocate new logic to phy map
  for ((intSpecWP, i) <- intRat.io.specWritePorts.zipWithIndex) {
    when(!io.robCommits.isWalk) {
      intSpecWP := io.intRenamePorts(i)
    }.otherwise{
      intSpecWP.wen := io.robCommits.isWalk && io.robCommits.walkValid(i) && io.robCommits.info(i).rfWen
      intSpecWP.addr := io.robCommits.info(i).ldest
      intSpecWP.data := io.robCommits.info(i).pdest
    }
  }
  for ((fpSpecWP, i) <- fpRat.io.specWritePorts.zipWithIndex) {
    when(!io.robCommits.isWalk) {
      fpSpecWP := io.fpRenamePorts(i)
    }.otherwise {
      fpSpecWP.wen := io.robCommits.isWalk && io.robCommits.walkValid(i) && io.robCommits.info(i).fpWen
      fpSpecWP.addr := io.robCommits.info(i).ldest
      fpSpecWP.data := io.robCommits.info(i).pdest
    }
  }

  intRat.io.snpt := io.snpt
  fpRat.io.snpt := io.snpt

  // commit write arch table and read old_pdest in arch table
  for ((intArchWP, i) <- intRat.io.archWritePorts.zipWithIndex) {
    intArchWP.wen := io.robCommits.isCommit && io.robCommits.commitValid(i) && io.robCommits.info(i).rfWen
    intArchWP.addr := io.robCommits.info(i).ldest
    intArchWP.data := io.robCommits.info(i).pdest
    XSError(intArchWP.wen && intArchWP.addr === 0.U && intArchWP.data =/= 0.U, "pdest for $0 should be 0\n")
  }

  for ((fpArchWP, i) <- fpRat.io.archWritePorts.zipWithIndex) {
    fpArchWP.wen := io.robCommits.isCommit && io.robCommits.commitValid(i) && io.robCommits.info(i).fpWen
    fpArchWP.addr := io.robCommits.info(i).ldest
    fpArchWP.data := io.robCommits.info(i).pdest
  }

  io.int_old_pdest := intRat.io.old_pdest
  io.fp_old_pdest := fpRat.io.old_pdest
  io.int_need_free := intRat.io.need_free
  io.fp_need_free := fpRat.io.need_free

  // arch state for debug
  io.debug_int_rat := intRat.io.debug_rdata
  io.debug_fp_rat := fpRat.io.debug_rdata
}