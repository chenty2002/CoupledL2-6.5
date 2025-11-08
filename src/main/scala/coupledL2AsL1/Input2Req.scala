package coupledL2AsL1

import chisel3._
import coupledL2.prefetch._
import org.chipsalliance.cde.config.Parameters

case class InputAsPrefectchParam() extends PrefetchParameters {
  override val hasPrefetchBit: Boolean = true
  override val hasPrefetchSrc: Boolean = true
  override val inflightEntries: Int = 16
}

object Input2ReqPfSource {
  val PrefetchRelease = PfSource.Stride.id.U
  val PrefetchAcquire = PfSource.TP.id.U
}

object ActiveReleaseParam {
  val toN = 0.U(1.W)
  val toB = 1.U(1.W)
}

class Input2Req(implicit p: Parameters) extends Prefetcher {
  val io_inputAddr      = IO(Input(UInt(fullAddressBits.W)))
  val io_inputNeedT     = IO(Input(Bool()))             // Acquire: need T; Release: 0->TtoN 1->TtoB
  val io_requestType    = IO(Input(Bool()))             // 0: Acquire; 1: Release

  val parsed = parseFullAddress(io_inputAddr)

  println("--------------------------------")
  println(" Modify Prefetcher as Input2Req ")
  println("--------------------------------")

  io.req.valid := true.B
  io.req.bits.tag := parsed._1
  io.req.bits.set := parsed._2
  io.req.bits.vaddr.foreach(_ := 0.U)
  io.req.bits.needT := io_inputNeedT
  io.req.bits.source := {
    // for Core 0, it's 0,2,4...; for Core 1, it's 1,3,5...
    val reqSource = RegInit(cacheParams.hartId.U(sourceIdBits.W))
    val releaseBit = (BigInt(1) << (sourceIdBits - 1)).U(sourceIdBits.W)
    when(io.req.valid && io.req.ready) {
      reqSource := reqSource + 2.U
    }
    Mux(io_requestType, reqSource | releaseBit, reqSource)
  }

  /*
  val NoWhere = Value("NoWhere")
  val SMS     = Value("SMS")
  val BOP     = Value("BOP")
  val PBOP     = Value("PBOP")
  val Stream  = Value("Stream")
  val Stride  = Value("Stride")
  val TP      = Value("TP")
  */
  io.req.bits.pfSource := Mux(io_requestType, Input2ReqPfSource.PrefetchRelease, Input2ReqPfSource.PrefetchAcquire)

  // train, resp, tlb_req are not used
  io.train.ready := true.B
  io.resp.ready := true.B

  io.tlb_req.req.valid := false.B
  io.tlb_req.req.bits := DontCare
  io.tlb_req.req_kill := DontCare
  io.tlb_req.resp.ready := true.B
}