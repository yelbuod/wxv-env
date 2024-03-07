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

package wenxuan.frontend.icache

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import utility._

class PrefetchMetaReadBundle(implicit p: Parameters) extends ICacheBundle {
  val idx = UInt(idxBits.W)
}

class PrefetchMetaRespBundle(implicit p: Parameters) extends ICacheBundle {
  val metaData   = Vec(nWays, new ICacheMetadata)
  val errors     = Vec(nWays ,Bool())
  val entryValid = Vec(nWays, Bool())

  def tags = VecInit(metaData.map(way => way.tag))
}

class ICacheMetaArrayNoBanked()(implicit p: Parameters) extends ICacheArray
{
  def onReset = ICacheMetadata(0.U)
  val metaBits = onReset.getWidth
  val metaEntryBits = cacheParams.tagCode.width(metaBits)

  val io=IO{new Bundle{
    val read     = Flipped(DecoupledIO(new PrefetchMetaReadBundle))
    val readResp = Output(new PrefetchMetaRespBundle)
    val write    = Flipped(DecoupledIO(new ICacheMetaWriteBundle))
    // val cacheOp  = Flipped(new L1CacheInnerOpIO)
    val fencei   = Input(Bool())
  }}

  io.read.ready := !io.write.valid

  val write_meta_bits = Wire(UInt(metaEntryBits.W))

  val tagArray = Module(new SRAMTemplate(
    UInt(metaEntryBits.W),
    set = nSets,
    way = nWays,
    shouldReset = true,
    holdRead = true,
    singlePort = true
  ))
  tagArray.io.r.req.valid := io.read.valid
  tagArray.io.r.req.bits.apply(setIdx=io.read.bits.idx)
  tagArray.io.w.req.valid := io.write.valid
  tagArray.io.w.req.bits.apply(data=write_meta_bits, setIdx=io.write.bits.virIdx, waymask=io.write.bits.waymask)

  val read_set_idx_next = RegEnable(io.read.bits.idx, io.read.fire)
  val valid_array = RegInit(VecInit(Seq.fill(nWays)(0.U(nSets.W))))
  val valid_metas = Wire(Vec(nWays, Bool()))
  (0 until nWays).foreach( way =>
    valid_metas(way) := valid_array(way)(read_set_idx_next)
  )
  io.readResp.entryValid := valid_metas

  // Parity Decode
  val read_metas = Wire(Vec(nWays,new ICacheMetadata()))
  val read_meta_bits = tagArray.io.r.resp.asTypeOf(Vec(nWays,UInt(metaEntryBits.W)))
  val read_meta_decoded = read_meta_bits.map{ way_bits => cacheParams.tagCode.decode(way_bits)}
  val read_meta_wrong = read_meta_decoded.map{ way_bits_decoded => way_bits_decoded.error}
  val read_meta_corrected = VecInit(read_meta_decoded.map{ way_bits_decoded => way_bits_decoded.corrected})
  read_metas := read_meta_corrected.asTypeOf(Vec(nWays,new ICacheMetadata()))
  (0 until nWays).map{ w => io.readResp.errors(w) := RegNext(read_meta_wrong(w)) && RegNext(RegNext(io.read.fire))}

  // Parity Encode
  val write = io.write.bits
  write_meta_bits := cacheParams.tagCode.encode(ICacheMetadata(tag = write.phyTag).asUInt)

  // valid write
  val way_num = OHToUInt(io.write.bits.waymask)
  when (io.write.valid) {
    valid_array(way_num) := valid_array(way_num).bitSet(io.write.bits.virIdx, true.B)
  }

//  XSPerfAccumulate("meta_refill_num", io.write.valid)

  io.readResp.metaData := read_metas

  io.write.ready := true.B // TODO : has bug ? should be !io.cacheOp.req.valid
  // deal with customized cache op
//  require(nWays <= 32)
//  io.cacheOp.resp.bits := DontCare
//  val cacheOpShouldResp = WireInit(false.B)
//  when(io.cacheOp.req.valid){
//    when(
//      CacheInstrucion.isReadTag(io.cacheOp.req.bits.opCode) ||
//        CacheInstrucion.isReadTagECC(io.cacheOp.req.bits.opCode)
//    ){
//      tagArray.io.r.req.valid := true.B
//      tagArray.io.r.req.bits.apply(setIdx = io.cacheOp.req.bits.index)
//      cacheOpShouldResp := true.B
//    }
//    when(CacheInstrucion.isWriteTag(io.cacheOp.req.bits.opCode)){
//      tagArray.io.w.req.valid := true.B
//      tagArray.io.w.req.bits.apply(
//        data = io.cacheOp.req.bits.write_tag_low,
//        setIdx = io.cacheOp.req.bits.index,
//        waymask = UIntToOH(io.cacheOp.req.bits.wayNum(log2Ceil(nWays) - 1, 0))
//      )
//      cacheOpShouldResp := true.B
//    }
//    // TODO
//    // when(CacheInstrucion.isWriteTagECC(io.cacheOp.req.bits.opCode)){
//    //   for (i <- 0 until readPorts) {
//    //     array(i).io.ecc_write.valid := true.B
//    //     array(i).io.ecc_write.bits.idx := io.cacheOp.req.bits.index
//    //     array(i).io.ecc_write.bits.way_en := UIntToOH(io.cacheOp.req.bits.wayNum(4, 0))
//    //     array(i).io.ecc_write.bits.ecc := io.cacheOp.req.bits.write_tag_ecc
//    //   }
//    //   cacheOpShouldResp := true.B
//    // }
//  }
//  io.cacheOp.resp.valid := RegNext(io.cacheOp.req.valid && cacheOpShouldResp)
//  io.cacheOp.resp.bits.read_tag_low := Mux(io.cacheOp.resp.valid,
//    tagArray.io.r.resp.asTypeOf(Vec(nWays, UInt(tagBits.W)))(io.cacheOp.req.bits.wayNum),
//    0.U
//  )
//  io.cacheOp.resp.bits.read_tag_ecc := DontCare // TODO
  // TODO: deal with duplicated array

  // fencei logic : reset valid_array
  when (io.fencei) {
    (0 until nWays).foreach( way =>
      valid_array(way) := 0.U
    )
  }
}
