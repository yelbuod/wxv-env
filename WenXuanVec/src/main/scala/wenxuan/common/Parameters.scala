package wenxuan.common

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.{Parameters, Field}

case class WXCoreParams(
	fetchWidth: Int = 4,
  decodeWidth: Int = 2,
  numRobEntries: Int = 128
)