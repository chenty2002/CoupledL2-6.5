package coupledL2AsL1

import chisel3._
import coupledL2.tl2tl.Slice
import org.chipsalliance.cde.config.Parameters

class L1Slice()(implicit p: Parameters) extends Slice {
  override lazy val sourceC = Module(new L1SourceC())
}
