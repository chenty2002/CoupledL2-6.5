package coupledL2AsL1

import chisel3._
import chisel3.util._
import coupledL2._
import coupledL2.tl2tl.SourceC
import freechips.rocketchip.tilelink.{LFSRNoiseMaker, TLBundleC}
import huancun.DirtyKey
import org.chipsalliance.cde.config.Parameters

class L1SourceC()(implicit p: Parameters) extends SourceC {
  override def toTLBundleC(task: TaskBundle, data: UInt = 0.U) = {
    val c = Wire(new TLBundleC(edgeOut.bundle))
    c.opcode := task.opcode
    c.param := task.param
    c.size := offsetBits.U
    c.source := task.mshrId
    c.address := Cat(task.tag, task.set, task.off)
    c.data := LFSRNoiseMaker(256)
    c.corrupt := false.B
    c.user.lift(utility.ReqSourceKey).foreach(_ := task.reqSource)
    c.echo.lift(DirtyKey).foreach(_ := task.dirty)
    c.echo.lift(L2AddrKey).foreach(_ := 0.U)
    c
  }
}
