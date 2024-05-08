package wenxuan

import chisel3._
import chisel3.util._

package object commonType {
  object CommitType {
    def NORMAL = "b000".U // int/fp

    def BRANCH = "b001".U // branch

    def LOAD = "b010".U // load

    def STORE = "b011".U // store

    def apply() = UInt(3.W)

    def isFused(commitType: UInt): Bool = commitType(2)

    def isLoadStore(commitType: UInt): Bool = !isFused(commitType) && commitType(1)

    def lsInstIsStore(commitType: UInt): Bool = commitType(0)

    def isStore(commitType: UInt): Bool = isLoadStore(commitType) && lsInstIsStore(commitType)

    def isBranch(commitType: UInt): Bool = commitType(0) && !commitType(1) && !isFused(commitType)
  }

  object TopDownCounters extends Enumeration {
    val NoStall = Value("NoStall") // Base
    // frontend
    val OverrideBubble = Value("OverrideBubble")
    val FtqUpdateBubble = Value("FtqUpdateBubble")
    // val ControlRedirectBubble = Value("ControlRedirectBubble")
    val TAGEMissBubble = Value("TAGEMissBubble")
    val SCMissBubble = Value("SCMissBubble")
    val ITTAGEMissBubble = Value("ITTAGEMissBubble")
    val RASMissBubble = Value("RASMissBubble")
    val MemVioRedirectBubble = Value("MemVioRedirectBubble")
    val OtherRedirectBubble = Value("OtherRedirectBubble")
    val FtqFullStall = Value("FtqFullStall")

    val ICacheMissBubble = Value("ICacheMissBubble")
    val ITLBMissBubble = Value("ITLBMissBubble")
    val BTBMissBubble = Value("BTBMissBubble")
    val FetchFragBubble = Value("FetchFragBubble")

    // backend
    // long inst stall at rob head
    val DivStall = Value("DivStall") // int div, float div/sqrt
    val IntNotReadyStall = Value("IntNotReadyStall") // int-inst at rob head not issue
    val FPNotReadyStall = Value("FPNotReadyStall") // fp-inst at rob head not issue
    val MemNotReadyStall = Value("MemNotReadyStall") // mem-inst at rob head not issue
    // freelist full
    val IntFlStall = Value("IntFlStall")
    val FpFlStall = Value("FpFlStall")
    // dispatch queue full
    val IntDqStall = Value("IntDqStall")
    val FpDqStall = Value("FpDqStall")
    val LsDqStall = Value("LsDqStall")

    // memblock
    val LoadTLBStall = Value("LoadTLBStall")
    val LoadL1Stall = Value("LoadL1Stall")
    val LoadL2Stall = Value("LoadL2Stall")
    val LoadL3Stall = Value("LoadL3Stall")
    val LoadMemStall = Value("LoadMemStall")
    val StoreStall = Value("StoreStall") // include store tlb miss
    val AtomicStall = Value("AtomicStall") // atomic, load reserved, store conditional

    // xs replay (different to gem5)
    val LoadVioReplayStall = Value("LoadVioReplayStall")
    val LoadMSHRReplayStall = Value("LoadMSHRReplayStall")

    // bad speculation
    val ControlRecoveryStall = Value("ControlRecoveryStall")
    val MemVioRecoveryStall = Value("MemVioRecoveryStall")
    val OtherRecoveryStall = Value("OtherRecoveryStall")

    val FlushedInsts = Value("FlushedInsts") // control flushed, memvio flushed, others

    val OtherCoreStall = Value("OtherCoreStall")

    val NumStallReasons = Value("NumStallReasons")
  }
}

package object backendInfoType {
  object RedirectLevel {
    def flushAfter = "b0".U // flush instrs after the instr itself

    def flush = "b1".U

    def apply() = UInt(1.W)

    // def isUnconditional(level: UInt) = level(1)
    def flushItself(level: UInt) = level(0) // need to flush instr itself, like ldReplay
    // def isException(level: UInt) = level(1) && level(0)
  }
}