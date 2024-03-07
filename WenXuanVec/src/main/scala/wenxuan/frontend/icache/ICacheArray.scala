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

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility._

class ICacheMetadata(implicit p: Parameters) extends ICacheBundle {
  val tag = UInt(tagBits.W)
}

object ICacheMetadata {
  def apply(tag: Bits)(implicit p: Parameters) = {
    val meta = Wire(new ICacheMetadata)
    meta.tag := tag
    meta
  }
}


class ICacheMetaArray()(implicit p: Parameters) extends ICacheArray
{
  def onReset = ICacheMetadata(0.U)
  val metaBits = onReset.getWidth
  // get the real width of tag after ECC coding
  val metaEntryBits = cacheParams.tagCode.width(metaBits)

  val io=IO{new Bundle{
    val write    = Flipped(DecoupledIO(new ICacheMetaWriteBundle))
    val read     = Flipped(DecoupledIO(new ICacheReadBundle))
    val readResp = Output(new ICacheMetaRespBundle)
    // val cacheOp  = Flipped(new L1CacheInnerOpIO) // customized cache op port
    val fencei   = Input(Bool())
  }}

  io.read.ready := !io.write.valid

  // bank select
  // since port_1 nextline to port_0, one of this two accesses must be odd and the other even.
  // and this two accesses can be allocated to the two banks without conflicit
  val port_0_read_0 = io.read.valid  && !io.read.bits.vSetIdx(0)(0)
  val port_0_read_1 = io.read.valid  &&  io.read.bits.vSetIdx(0)(0)
  val port_1_read_0  = io.read.valid && !io.read.bits.vSetIdx(1)(0) && io.read.bits.isDoubleLine
  val port_1_read_1  = io.read.valid &&  io.read.bits.vSetIdx(1)(0) && io.read.bits.isDoubleLine

  val port_0_read_0_reg = RegEnable(port_0_read_0, io.read.fire)
  val port_0_read_1_reg = RegEnable(port_0_read_1, io.read.fire)
  val port_1_read_1_reg = RegEnable(port_1_read_1, io.read.fire)
  val port_1_read_0_reg = RegEnable(port_1_read_0, io.read.fire)

  val bank_0_idx = Mux(port_0_read_0, io.read.bits.vSetIdx(0), io.read.bits.vSetIdx(1))
  val bank_1_idx = Mux(port_0_read_1, io.read.bits.vSetIdx(0), io.read.bits.vSetIdx(1))
  val bank_idx   = Seq(bank_0_idx, bank_1_idx)

  val write_bank_0 = io.write.valid && !io.write.bits.bankIdx
  val write_bank_1 = io.write.valid &&  io.write.bits.bankIdx

  val write_meta_bits = Wire(UInt(metaEntryBits.W))

  // equivalent to splitting the set of odd-even addresses in half as two banks
  val tagArrays = (0 until 2) map { bank =>
    val tagArray = Module(new SRAMTemplate(
      UInt(metaEntryBits.W),
      set=nSets/2,
      way=nWays,
      shouldReset = true,
      holdRead = true,
      singlePort = true
    ))

    //meta connection
    if(bank == 0) {
      tagArray.io.r.req.valid := port_0_read_0 || port_1_read_0
      tagArray.io.r.req.bits.apply(setIdx=bank_0_idx(highestIdxBit,1))
      tagArray.io.w.req.valid := write_bank_0
      tagArray.io.w.req.bits.apply(data=write_meta_bits, setIdx=io.write.bits.virIdx(highestIdxBit,1), waymask=io.write.bits.waymask)
    }
    else {
      tagArray.io.r.req.valid := port_0_read_1 || port_1_read_1
      tagArray.io.r.req.bits.apply(setIdx=bank_1_idx(highestIdxBit,1))
      tagArray.io.w.req.valid := write_bank_1
      tagArray.io.w.req.bits.apply(data=write_meta_bits, setIdx=io.write.bits.virIdx(highestIdxBit,1), waymask=io.write.bits.waymask)
    }

    tagArray
  }

  // Access valid Array
  val read_set_idx_next = RegEnable(io.read.bits.vSetIdx, io.read.fire)
  val valid_array = RegInit(VecInit(Seq.fill(nWays)(0.U(nSets.W))))
  val valid_metas = Wire(Vec(PortNumber, Vec(nWays, Bool())))
  // valid read
  (0 until PortNumber).foreach( i =>
    (0 until nWays).foreach( way =>
      valid_metas(i)(way) := valid_array(way)(read_set_idx_next(i))
    ))
  io.readResp.entryValid := valid_metas

  io.read.ready := !io.write.valid && !io.fencei && tagArrays.map(_.io.r.req.ready).reduce(_&&_)

  //Parity Decode
  val read_metas = Wire(Vec(2,Vec(nWays,new ICacheMetadata())))
  for((tagArray,i) <- tagArrays.zipWithIndex){
    val read_meta_bits = tagArray.io.r.resp.asTypeOf(Vec(nWays,UInt(metaEntryBits.W)))
    val read_meta_decoded = read_meta_bits.map{ way_bits => cacheParams.tagCode.decode(way_bits)}
    val read_meta_wrong = read_meta_decoded.map{ way_bits_decoded => way_bits_decoded.error}
    val read_meta_corrected = VecInit(read_meta_decoded.map{ way_bits_decoded => way_bits_decoded.corrected})
    read_metas(i) := read_meta_corrected.asTypeOf(Vec(nWays,new ICacheMetadata()))
    // read.fire -> tagArray.io.r.resp + read_meta_wrong... -> io.readResp.errors
    // two cycle delay because data error delay a cycle in ICacheMainPipe s2 stage
    // so meta error delay here to sync with data error
    (0 until nWays).map{ w => io.readResp.errors(i)(w) := RegNext(read_meta_wrong(w)) && RegNext(RegNext(io.read.fire))}
  }

  // Parity Encode
  val write = io.write.bits
  write_meta_bits := cacheParams.tagCode.encode(ICacheMetadata(tag = write.phyTag).asUInt)

  // valid write
  val way_num = OHToUInt(io.write.bits.waymask)
  when (io.write.valid) {
    valid_array(way_num) := valid_array(way_num).bitSet(io.write.bits.virIdx, true.B)
  }

//  XSPerfAccumulate("meta_refill_num", io.write.valid)

  io.readResp.metaData <> DontCare
  when(port_0_read_0_reg){
    io.readResp.metaData(0) := read_metas(0)
  }.elsewhen(port_0_read_1_reg){
    io.readResp.metaData(0) := read_metas(1)
  }

  when(port_1_read_0_reg){
    io.readResp.metaData(1) := read_metas(0)
  }.elsewhen(port_1_read_1_reg){
    io.readResp.metaData(1) := read_metas(1)
  }


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
//      for (i <- 0 until 2) {
//        tagArrays(i).io.r.req.valid := true.B
//        tagArrays(i).io.r.req.bits.apply(setIdx = io.cacheOp.req.bits.index)
//      }
//      cacheOpShouldResp := true.B
//    }
//    when(CacheInstrucion.isWriteTag(io.cacheOp.req.bits.opCode)){
//      for (i <- 0 until 2) {
//        tagArrays(i).io.w.req.valid := true.B
//        tagArrays(i).io.w.req.bits.apply(
//          data = io.cacheOp.req.bits.write_tag_low,
//          setIdx = io.cacheOp.req.bits.index,
//          waymask = UIntToOH(io.cacheOp.req.bits.wayNum(log2Ceil(nWays) - 1, 0))
//        )
//      }
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
//    tagArrays(0).io.r.resp.asTypeOf(Vec(nWays, UInt(tagBits.W)))(io.cacheOp.req.bits.wayNum),
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



class ICacheDataArray(implicit p: Parameters) extends ICacheArray
{

  def getECCFromEncUnit(encUnit: UInt) = {
    require(encUnit.getWidth == encDataUnitBits)
    if (encDataUnitBits == dataCodeUnit) {
      0.U.asTypeOf(UInt(1.W))
    } else {
      encUnit(encDataUnitBits - 1, dataCodeUnit)
    }
  }

  def getECCFromBlock(cacheblock: UInt) = {
    // require(cacheblock.getWidth == blockBits)
    // extract each ECC code unit in the block for encoding and return the encode-bit part from encode data
    VecInit((0 until dataCodeUnitNum).map { w =>
      val unit = cacheblock(dataCodeUnit * (w + 1) - 1, dataCodeUnit * w)
      // encode the data and then extract the encode part
      getECCFromEncUnit(cacheParams.dataCode.encode(unit))
    })
  }

  val codeBits = dataCodeEntryBits

  val io=IO{new Bundle{
    val write    = Flipped(DecoupledIO(new ICacheDataWriteBundle))
    val read     = Flipped(DecoupledIO(Vec(partWayNum, new ICacheReadBundle)))
    val readResp = Output(new ICacheDataRespBundle)
    // val cacheOp  = Flipped(new L1CacheInnerOpIO) // customized cache op port
  }}
  // io.cacheOp := DontCare
  /**
   ******************************************************************************
   * data array
   ******************************************************************************
   */
  val write_data_bits = io.write.bits.data.asTypeOf(Vec(2, UInt(partBlockBits.W)))
  val dataArrays = (0 until partWayNum).map{ bank =>
    (0 until 2).map { i =>
      val sramBank = Module(new SRAMTemplate(
        UInt(partBlockBits.W),
        set=nSets,
        way=pWay,
        shouldReset = true,
        holdRead = true,
        singlePort = true
      ))
      // SRAM read logic
      sramBank.io.r.req.valid := io.read.valid
      if (i == 1) {
        sramBank.io.r.req.bits.apply(setIdx= io.read.bits(bank).vSetIdx(0))
      } else {
        // read low of startline if cross cacheline
        val setIdx = Mux(io.read.bits(bank).isDoubleLine, io.read.bits(bank).vSetIdx(1), io.read.bits(bank).vSetIdx(0))
        sramBank.io.r.req.bits.apply(setIdx= setIdx)
      }

      // SRAM write logic
      val waymask = io.write.bits.waymask.asTypeOf(Vec(partWayNum, Vec(pWay, Bool())))(bank)
      // waymask is invalid when way of SRAMTemplate is 1
      sramBank.io.w.req.valid := io.write.valid && waymask.asUInt.orR
      sramBank.io.w.req.bits.apply(
        data    = write_data_bits(i),
        setIdx  = io.write.bits.virIdx,
        waymask = waymask.asUInt
      )
      sramBank
    }
  }

  /**
   ******************************************************************************
   * data code array
   ******************************************************************************
   */
  val write_code_bits = write_data_bits.map(getECCFromBlock(_).asUInt)
  val codeArrays = (0 until 2) map { i =>
    val codeArray = Module(new SRAMTemplate(
      UInt(codeBits.W),
      set=nSets,
      way=nWays,
      shouldReset = true,
      holdRead = true,
      singlePort = true
    ))
    // SRAM read logic
    codeArray.io.r.req.valid := io.read.valid
    if (i == 1) {
      codeArray.io.r.req.bits.apply(setIdx= io.read.bits.last.vSetIdx(0))
    } else {
      val setIdx = Mux(io.read.bits.last.isDoubleLine, io.read.bits.last.vSetIdx(1), io.read.bits.last.vSetIdx(0))
      codeArray.io.r.req.bits.apply(setIdx= setIdx)
    }
    // SRAM write logic
    codeArray.io.w.req.valid := io.write.valid
    codeArray.io.w.req.bits.apply(
      data    = write_code_bits(i),
      setIdx  = io.write.bits.virIdx,
      waymask = io.write.bits.waymask
    )
    codeArray
  }

  /**
   ******************************************************************************
   * read logic
   ******************************************************************************
   */
  val isDoubleLineReg = RegEnable(io.read.bits.last.isDoubleLine, io.read.fire)
  val read_data_bits = Wire(Vec(2,Vec(nWays,UInt(partBlockBits.W))))
  val read_code_bits = Wire(Vec(2,Vec(nWays,UInt(codeBits.W))))

  // when isDoubleLine is true, NO.0 read must be in the upper half of the cacheline
  (0 until nWays).map { w =>
    // first data
    read_data_bits(0)(w) := Mux(isDoubleLineReg,
      dataArrays(w/pWay)(1).io.r.resp.asTypeOf(Vec(pWay, UInt(partBlockBits.W)))(w%pWay),
      dataArrays(w/pWay)(0).io.r.resp.asTypeOf(Vec(pWay, UInt(partBlockBits.W)))(w%pWay))
    // second data
    read_data_bits(1)(w) := Mux(isDoubleLineReg,
      dataArrays(w/pWay)(0).io.r.resp.asTypeOf(Vec(pWay, UInt(partBlockBits.W)))(w%pWay),
      dataArrays(w/pWay)(1).io.r.resp.asTypeOf(Vec(pWay, UInt(partBlockBits.W)))(w%pWay))
  }
  // first data code
  read_code_bits(0) := Mux(isDoubleLineReg,
    codeArrays(1).io.r.resp.asTypeOf(Vec(nWays, UInt(codeBits.W))),
    codeArrays(0).io.r.resp.asTypeOf(Vec(nWays, UInt(codeBits.W))))
  // second data code
  read_code_bits(1) := Mux(isDoubleLineReg,
    codeArrays(0).io.r.resp.asTypeOf(Vec(nWays, UInt(codeBits.W))),
    codeArrays(1).io.r.resp.asTypeOf(Vec(nWays, UInt(codeBits.W))))

  if (ICacheECCForceError) {
    read_code_bits.foreach(_.foreach(_ := 0.U)) // force ecc to fail
  }

  /**
   ******************************************************************************
   * IO
   ******************************************************************************
   */
  io.readResp.datas := read_data_bits
  io.readResp.codes := read_code_bits
  io.write.ready := true.B
  io.read.ready := !io.write.valid &&
    dataArrays.map(_.map(_.io.r.req.ready).reduce(_&&_)).reduce(_&&_) &&
    codeArrays.map(_.io.r.req.ready).reduce(_&&_)
}
