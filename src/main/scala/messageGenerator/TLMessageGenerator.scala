package messageGenerator

import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util.{BundleFieldBase, BundleKeyBase}
import org.chipsalliance.cde.config.Parameters

case class MessageGeneratorParam(
  name: String = "MsgGen",
  sets: Int = 16,
  ways: Int = 1,
  blockBytes: Int = 64,
  channelBytes: TLChannelBeatBytes = TLChannelBeatBytes(32),
  
  // TileLink client parameters
  sourceIdRange: IdRange = IdRange(0, 16),
  reqField: Seq[BundleFieldBase] = Nil,
  respKey: Seq[BundleKeyBase] = Nil,
  supportsProbe: Option[TransferSizes] = None,
  minLatency: Int = 1
) {
  // Derived values for compatibility
  def lines: Int = sets * ways
  def clientName: String = name
  def requestFields: Seq[BundleFieldBase] = reqField
  def responseKeys: Seq[BundleKeyBase] = respKey
}

class TLMessageGenerator(params: MessageGeneratorParam)(implicit p: Parameters) extends LazyModule {
  private val blockBytes = params.blockBytes
  private val supportsProbe = params.supportsProbe.getOrElse(TransferSizes(blockBytes))
  private val maxSources = params.sourceIdRange.size
  require(maxSources > 0 && maxSources <= 16, "TLMessageGenerator expects 1 to 16 sourceIds")

  val node = TLClientNode(Seq(
    TLMasterPortParameters.v2(
      masters = Seq(
        TLMasterParameters.v1(
          name = params.name,
          sourceId = params.sourceIdRange,
          supportsProbe = supportsProbe
        )
      ),
      channelBytes = params.channelBytes,
      minLatency = params.minLatency,
      requestFields = params.reqField,
      echoFields = Nil,
      responseKeys = params.respKey
    )
  ))

  class TLMessageGeneratorImp(wrapper: TLMessageGenerator) extends LazyModuleImp(wrapper) {
    val out  = node.out.head._1
    val edge = node.out.head._2

    val io = IO(new Bundle {
      val in_addr       = Input(UInt(edge.bundle.addressBits.W))
      val in_isAcquire  = Input(Bool())
      val in_param      = Input(Bool())
    })

    val lines = params.lines
    val beatBytes = edge.bundle.dataBits / 8
    require(blockBytes % beatBytes == 0, s"blockBytes ($blockBytes) must be a multiple of beatBytes ($beatBytes)")
    val blockBits = blockBytes * 8
    val lineBeats = (blockBytes / beatBytes) max 1
    val beatBits  = beatBytes * 8
    val beatAddrShift = log2Ceil(beatBytes)
    val beatCountBits = scala.math.max(log2Ceil(lineBeats), 1)
    val offBits   = log2Ceil(blockBytes)
    val idxBits   = log2Ceil(lines)
    val tagBits   = edge.bundle.addressBits - idxBits - offBits
    val numSources = maxSources
    val sourceIdVec = VecInit((0 until numSources).map(i => (params.sourceIdRange.start + i).U(edge.bundle.sourceBits.W)))
    val defaultSourceId = params.sourceIdRange.start.U(edge.bundle.sourceBits.W)

    private val releaseSourceC = Module(new MsgGenSourceC(edge, blockBytes, beatBytes))
    releaseSourceC.io.source := defaultSourceId

    def addrIndex(a: UInt) = (a >> offBits)(idxBits-1,0)
    def addrTag(a: UInt)   = (a >> (offBits + idxBits))(tagBits-1,0)
    def lineBase(a: UInt)  = Cat(a(edge.bundle.addressBits-1, offBits), 0.U(offBits.W))

    val dir_valid = RegInit(VecInit(Seq.fill(lines)(false.B)))
    val dir_tag   = Reg(Vec(lines, UInt(tagBits.W)))
    val dir_state = RegInit(VecInit(Seq.fill(lines)(0.U(2.W))))
    val dir_data  = Reg(Vec(lines, UInt(blockBits.W)))

    val multiBeatLine = lineBeats > 1
    val probeActive       = RegInit(false.B)
    val probeBeatIdx      = RegInit(0.U(beatCountBits.W))
    val probeStoredData   = RegInit(0.U(blockBits.W))
    val probeStoredBase   = RegInit(0.U(edge.bundle.addressBits.W))

    val entryIdle :: entrySendAcquire :: entryWaitGrant :: entryGrantAck :: Nil = Enum(4)
    // Each entry tracks one outstanding request keyed by its TileLink source ID.
    val entryActive = RegInit(VecInit(Seq.fill(numSources)(false.B)))
    val entryState = RegInit(VecInit(Seq.fill(numSources)(entryIdle)))
    val entryNeedT = Reg(Vec(numSources, Bool()))
    val entryAddr = Reg(Vec(numSources, UInt(edge.bundle.addressBits.W)))
    val entryGrantAccumulating = RegInit(VecInit(Seq.fill(numSources)(false.B)))
    val entryGrantBeatIdx = RegInit(VecInit(Seq.fill(numSources)(0.U(beatCountBits.W))))
    
    val entryGrantBeats = Reg(Vec(numSources, Vec(lineBeats, UInt(beatBits.W))))
    val entryGrantSink = if (edge.bundle.sinkBits > 0) Some(RegInit(VecInit(Seq.fill(numSources)(0.U(edge.bundle.sinkBits.W))))) else None

    
    private val releaseReqReg   = Reg(new MsgGenReleaseReq(edge, blockBytes))
    val releaseReqValid = RegInit(false.B)

    val releaseIdle :: releaseEnqueue :: releaseWaitAck :: Nil = Enum(3)
    val releaseState = RegInit(releaseIdle)

    val takeAcquire = io.in_isAcquire
    val takeRelease = !io.in_isAcquire
    val takeAddrIndex = addrIndex(io.in_addr)
    val takeAddrTag   = addrTag(io.in_addr)
    val takeLineHit   = dir_valid(takeAddrIndex) && dir_tag(takeAddrIndex) === takeAddrTag
    val freeVec = entryActive.map(!_)
    val freeMask = VecInit(freeVec).asUInt
    val allocateOH = PriorityEncoderOH(freeMask)
    val canAllocate = freeMask.orR
    val allocateAcquire = takeAcquire && canAllocate
    // Optimized: use OHToUInt directly
    val allocIdx = OHToUInt(allocateOH)

    when (allocateAcquire) {
      entryActive(allocIdx) := true.B
      entryState(allocIdx) := entrySendAcquire
      entryNeedT(allocIdx) := io.in_param
      entryAddr(allocIdx) := io.in_addr
      entryGrantAccumulating(allocIdx) := false.B
      entryGrantBeatIdx(allocIdx) := 0.U
    }

    // Requests that arrive when no slots are free are silently dropped per spec.

    when (takeRelease && takeLineHit && releaseState === releaseIdle) {
      releaseReqReg.address := lineBase(io.in_addr)
      releaseReqReg.param   := Mux(io.in_param, TLPermissions.TtoB, TLPermissions.TtoN)
      releaseReqReg.data    := dir_data(takeAddrIndex)
      releaseReqValid       := true.B
      releaseState := releaseEnqueue
    }

    // Assume: Release requests must hit a valid cache line
    assume(io.in_isAcquire || takeLineHit)

    // Optimized: use OHToUInt directly instead of Mux1H
    val sendAcquireVec = VecInit((0 until numSources).map(i => entryActive(i) && entryState(i) === entrySendAcquire))
    val sendAcquireMask = sendAcquireVec.asUInt
    val sendAcquireSel = PriorityEncoderOH(sendAcquireMask)
    val hasAcquireToSend = sendAcquireMask.orR
    val acquireIndex = OHToUInt(sendAcquireSel)

    val aBitsWire = Wire(out.a.bits.cloneType)
    aBitsWire := 0.U.asTypeOf(out.a.bits)
    when (hasAcquireToSend) {
      val growPerm = Mux(entryNeedT(acquireIndex), TLPermissions.NtoT, TLPermissions.NtoB)
      val (_, acquireMsg) = edge.AcquireBlock(sourceIdVec(acquireIndex), lineBase(entryAddr(acquireIndex)), log2Ceil(blockBytes).U, growPerm)
      aBitsWire := acquireMsg
    }
    out.a.valid := false.B
    out.a.bits  := aBitsWire
    out.c.valid := false.B
    out.c.bits  := DontCare
    out.e.valid := false.B
    out.e.bits  := DontCare
    out.b.ready := !probeActive
    out.d.ready := true.B

    // Optimized: use decoded index instead of loop
    when (out.a.fire) {
      entryState(acquireIndex) := entryWaitGrant
    }

    val probeHitIndex = addrIndex(out.b.bits.address)
    val probeHitTag   = addrTag(out.b.bits.address)
    val probeHit      = dir_valid(probeHitIndex) && dir_tag(probeHitIndex) === probeHitTag
    val probeData     = dir_data(probeHitIndex)
    val probeState    = dir_state(probeHitIndex)
    val probeBaseAddr = lineBase(out.b.bits.address)
    val sendingProbeAck = out.b.valid && out.b.ready && probeHit
    val allowAcquire = hasAcquireToSend && !sendingProbeAck
    when (allowAcquire) {
      out.a.valid := true.B
    }

    val sourceCOut = releaseSourceC.io.tlc
    val cBitsWire = Wire(out.c.bits.cloneType)
    cBitsWire := sourceCOut.bits
    val cValidWire = WireInit(sourceCOut.valid)
    val sourceCReady = WireInit(out.c.ready && !sendingProbeAck && !probeActive)

    def mkProbeAckBundle(addr: UInt, data: UInt) = {
      val bundle = WireDefault(0.U.asTypeOf(out.c.bits))
      bundle.opcode := TLMessages.ProbeAckData
      bundle.param  := 0.U
      bundle.source := defaultSourceId
      bundle.address:= addr
      bundle.size   := log2Ceil(blockBytes).U
      bundle.data   := data
      bundle.corrupt:= false.B
      bundle
    }

    when (sendingProbeAck) {
      val probeVec = probeData.asTypeOf(Vec(lineBeats, UInt(beatBits.W)))
      val firstBeat = probeVec.head
      cBitsWire := mkProbeAckBundle(probeBaseAddr, firstBeat)
      cValidWire := true.B
      sourceCReady := false.B
      when (out.c.fire) {
        when (probeState === 2.U) { dir_state(probeHitIndex) := 1.U }
        if (multiBeatLine) {
          probeActive := true.B
          probeBeatIdx := 1.U
          probeStoredData := probeData
          probeStoredBase := probeBaseAddr
        } else {
          probeActive := false.B
          probeBeatIdx := 0.U
        }
      }
    }

    if (multiBeatLine) {
      val storedVec = probeStoredData.asTypeOf(Vec(lineBeats, UInt(beatBits.W)))
      when (probeActive && !sendingProbeAck) {
        val beatData = storedVec(probeBeatIdx)
        val beatAddr = probeStoredBase + (probeBeatIdx << beatAddrShift).asUInt
        cBitsWire := mkProbeAckBundle(beatAddr, beatData)
        cValidWire := true.B
        sourceCReady := false.B
        when (out.c.fire) {
          val lastBeat = probeBeatIdx === (lineBeats - 1).U
          when (lastBeat) {
            probeActive := false.B
            probeBeatIdx := 0.U
          }.otherwise {
            probeBeatIdx := probeBeatIdx + 1.U
          }
        }
      }
    } else {
      when (!sendingProbeAck) {
        probeActive := false.B
        probeBeatIdx := 0.U
      }
    }

    sourceCOut.ready := sourceCReady
    out.c.valid := cValidWire
    out.c.bits  := cBitsWire

    val releaseIndex = addrIndex(releaseReqReg.address)
    releaseSourceC.io.req.valid := (releaseState === releaseEnqueue) && releaseReqValid
    releaseSourceC.io.req.bits  := releaseReqReg

    when (releaseSourceC.io.req.fire) {
      releaseReqValid := false.B
      dir_valid(releaseIndex) := false.B
      dir_state(releaseIndex) := 0.U
      releaseState := releaseWaitAck
    }

    when (out.d.fire && out.d.bits.opcode === TLMessages.ReleaseAck) {
      releaseState := releaseIdle
      releaseReqValid := false.B
    }

    
    val dSourceMatch = VecInit((0 until numSources).map(i => sourceIdVec(i) === out.d.bits.source))
    val dMatchedIdx = OHToUInt(dSourceMatch.asUInt)
    val dMatched = dSourceMatch.asUInt.orR

    when (out.d.fire && dMatched) {
      // Use decoded index directly instead of iterating
      val matchedEntry = dMatchedIdx
      val lineIdx = addrIndex(entryAddr(matchedEntry))
      val lineTag = addrTag(entryAddr(matchedEntry))
      
      when (out.d.bits.opcode === TLMessages.GrantData) {
        // Directly update the matching beat without building full updatedBeats Wire
        entryGrantBeats(matchedEntry)(entryGrantBeatIdx(matchedEntry)) := out.d.bits.data
        
        when (!entryGrantAccumulating(matchedEntry)) {
          entryGrantAccumulating(matchedEntry) := true.B
          entryGrantBeatIdx(matchedEntry) := 0.U
        }

        when (edge.last(out.d)) {
          // Build fullLine only when complete
          val fullLine = if (lineBeats == 1) {
            entryGrantBeats(matchedEntry).head
          } else {
            Cat(entryGrantBeats(matchedEntry).reverse)
          }
          dir_valid(lineIdx) := true.B
          dir_tag(lineIdx)   := lineTag
          dir_state(lineIdx) := Mux(entryNeedT(matchedEntry), 2.U, 1.U)
          dir_data(lineIdx)  := fullLine
          entryGrantAccumulating(matchedEntry) := false.B
          entryGrantBeatIdx(matchedEntry) := 0.U
          entryState(matchedEntry) := entryGrantAck
          if (edge.bundle.sinkBits > 0) { entryGrantSink.get(matchedEntry) := out.d.bits.sink }
        }.otherwise {
          entryGrantBeatIdx(matchedEntry) := entryGrantBeatIdx(matchedEntry) + 1.U
        }
      }.elsewhen (out.d.bits.opcode === TLMessages.Grant) {
        dir_valid(lineIdx) := true.B
        dir_tag(lineIdx)   := lineTag
        dir_state(lineIdx) := Mux(entryNeedT(matchedEntry), 2.U, 1.U)
        dir_data(lineIdx)  := 0.U
        entryState(matchedEntry) := entryGrantAck
        entryGrantAccumulating(matchedEntry) := false.B
        entryGrantBeatIdx(matchedEntry) := 0.U
        if (edge.bundle.sinkBits > 0) { entryGrantSink.get(matchedEntry) := out.d.bits.sink }
      }
    }

    // Optimized: compute index once with OHToUInt
    val grantAckVec = VecInit((0 until numSources).map(i => entryActive(i) && entryState(i) === entryGrantAck))
    val grantAckMask = grantAckVec.asUInt
    val grantAckSel = PriorityEncoderOH(grantAckMask)
    val hasGrantAck = grantAckMask.orR
    val grantAckIdx = OHToUInt(grantAckSel)
    
    val eBitsWire = Wire(out.e.bits.cloneType)
    eBitsWire := 0.U.asTypeOf(out.e.bits)
    if (edge.bundle.sinkBits > 0) {
      when (hasGrantAck) { eBitsWire.sink := entryGrantSink.get(grantAckIdx) }
    }
    out.e.valid := hasGrantAck
    out.e.bits := eBitsWire

    // Optimized: use decoded index instead of loop
    when (out.e.fire) {
      entryActive(grantAckIdx) := false.B
      entryState(grantAckIdx) := entryIdle
    }

    when (!out.a.valid) { out.a.bits := 0.U.asTypeOf(out.a.bits) }
    when (!out.c.valid) { out.c.bits := 0.U.asTypeOf(out.c.bits) }

    val io_in_addr      = io.in_addr
    val io_in_isAcquire = io.in_isAcquire
    val io_in_param     = io.in_param
  }

  lazy val module = new TLMessageGeneratorImp(this)
}
