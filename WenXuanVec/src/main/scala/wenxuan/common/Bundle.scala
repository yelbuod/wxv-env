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

package wenxuan.common

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import wenxuan.cache.HasDCacheParameters
import utility._

/** Distribute to CSR signal bundle */
class DistributedCSRIO(implicit p: Parameters) extends WXBundle {
  // CSR has been written by csr inst, copies of csr should be updated
  val w = ValidIO(new Bundle {
    val addr = Output(UInt(12.W))
    val data = Output(UInt(XLEN.W))
  })
}


class DistributedCSRUpdateReq(implicit p: Parameters) extends WXBundle {
  // Request csr to be updated
  //
  // Note that this request will ONLY update CSR Module it self,
  // copies of csr will NOT be updated, use it with care!
  //
  // For each cycle, no more than 1 DistributedCSRUpdateReq is valid
  val w = ValidIO(new Bundle {
    val addr = Output(UInt(12.W))
    val data = Output(UInt(XLEN.W))
  })
  def apply(valid: Bool, addr: UInt, data: UInt, src_description: String) = {
    when(valid){
      w.bits.addr := addr
      w.bits.data := data
    }
    println("Distributed CSR update req registered for " + src_description)
  }
}

/** custom l2 - l1 interface */
class L2ToL1Hint(implicit p: Parameters) extends WXBundle with HasDCacheParameters {
  val sourceId = UInt(log2Up(cacheParams.nMissEntries).W)    // tilelink sourceID -> mshr id
  val isKeyword = Bool()                             // miss entry keyword -> L1 load queue replay
}


/** satp csr */
class SatpStruct(implicit p: Parameters) extends WXBundle {
  val mode = UInt(4.W) // virtual memory system mode, sv32/sv39/sv48 ...
  val asid = UInt(16.W)
  val ppn  = UInt(44.W)
}

class TlbSatpBundle(implicit p: Parameters) extends SatpStruct {
  val changed = Bool()

  def apply(satp_value: UInt): Unit = {
    require(satp_value.getWidth == XLEN)
    val sa = satp_value.asTypeOf(new SatpStruct)
    mode := sa.mode
    asid := sa.asid
    ppn := Cat(0.U((44-PAddrBits).W), sa.ppn(PAddrBits-1, 0)).asUInt
    changed := DataChanged(sa.asid) // when ppn is changed, software need do the flush
  }
}

class TlbCsrBundle(implicit p: Parameters) extends WXBundle {
  val satp = new TlbSatpBundle() // satp csr
  val priv = new Bundle{
    val mxr = Bool() // mxr field in mstatus csr, takes effect in privilege of load access
    val sum = Bool() // sum field in mstatus csr, takes effect in privilege of load & store access
    val imode = UInt(2.W) // privilege in instruction mode
    val dmode = UInt(2.W) // privilege in debug mode
  }
}

/** sfence  */
class SfenceBundle(implicit p: Parameters) extends WXBundle {
  val valid = Bool()
  val bits = new Bundle {
    val rs1 = Bool()
    val rs2 = Bool()
    val addr = UInt(VAddrBits.W)
    val asid = UInt(AsidLength.W)
    val flushPipe = Bool()
  }

  override def toPrintable: Printable = {
    p"valid:0x${Hexadecimal(valid)} rs1:${bits.rs1} rs2:${bits.rs2} addr:${Hexadecimal(bits.addr)}, flushPipe:${bits.flushPipe}"
  }
}

