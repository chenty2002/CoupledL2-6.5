package coupledL2Assume

import chisel3._
import circt.stage.ChiselStage
import chisel3.util._
import chisel3.util.experimental.BoringUtils
import chiselFv._
import coupledL2._
import coupledL2.tl2tl.{Slice => L2Slice, _}
import coupledL2AsL1._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tile.MaxHartIdBits
import freechips.rocketchip.tilelink.TLMessages.{ProbeAck, Release}
import freechips.rocketchip.tilelink._
import huancun._
import org.chipsalliance.cde.config._
import utility._


object baseConfig {
  def apply(maxHartIdBits: Int) = {
    new Config((_, _, _) => {
      case MaxHartIdBits => maxHartIdBits
    })
  }
}

class VerifyTop()(implicit p: Parameters) extends LazyModule {

  /* L1D   L1D
   *  |     |
   * L2    L2
   *  \    /
   *    L3
   */

  override lazy val desiredName: String = "VerifyTop"
  val delayFactor = 0.2
  val cacheParams = p(L2ParamKey)

  val nrL2 = 2

  def createClientNode(name: String, sources: Int) = {
    val masterNode = TLClientNode(Seq(
      TLMasterPortParameters.v2(
        masters = Seq(
          TLMasterParameters.v1(
            name = name,
            sourceId = IdRange(0, sources),
            supportsProbe = TransferSizes(cacheParams.blockBytes)
          )
        ),
        channelBytes = TLChannelBeatBytes(cacheParams.blockBytes),
        minLatency = 1,
        echoFields = Nil,
        requestFields = Seq(AliasField(2)),
        responseKeys = cacheParams.respKey
      )
    ))
    masterNode
  }
  val l0_nodes = (0 until nrL2).map(i => createClientNode(s"L0_$i", 32))

  val coupledL2AsL1 = (0 until nrL2).map(i => LazyModule(new TLCoupledL2AsL1()(baseConfig(1).alter((_, here, _) => {
    case L2ParamKey => L2Param(
      name = s"L1d_$i",
      ways = 4,
      sets = 128,
      clientCaches = Seq(L1Param(aliasBitsOpt = Some(2))),
      // echoField = Seq(DirtyField()),
      hartId = i,
      prefetch = Option(InputAsPrefectchParam())
    )
    case huancun.BankBitsKey => 0 // FV: 1 bank for L1s
  })))
  )
  val l1d_nodes = coupledL2AsL1.map(_.node)

  val coupledL2 = (0 until nrL2).map(i => LazyModule(new TL2TLCoupledL2()(baseConfig(1).alter((_, here, _) => {
    case L2ParamKey => L2Param(
      name = s"l2$i",
      ways = 4,
      sets = 128,
      clientCaches = Seq(L1Param(aliasBitsOpt = Some(2))),
      echoField = Seq(DirtyField(), L2AddrField()),
      hartId = i,
    )
    case huancun.BankBitsKey => 0
  }))))
  val l2_nodes = coupledL2.map(_.node)

  val l3 = LazyModule(new HuanCun()(baseConfig(1).alter((_, here, _) => {
    case HCCacheParamsKey => HCCacheParameters(
      name = "L3",
      level = 3,
      ways = 4,
      sets = 128,
      inclusive = false,
      clientCaches = (0 until nrL2).map(_ =>
        CacheParameters(
          name = s"l2",
          sets = 128,
          ways = 4 + 2,
          blockGranularity = log2Ceil(128)
        ),
      ),
      echoField = Seq(DirtyField()),
      simulation = true
    )
  })))

  val xbar = TLXbar()
  val ram = LazyModule(new TLRAM(AddressSet(0, 0xff_ffffL), beatBytes = 32))

  l0_nodes.zip(l1d_nodes) map {
    case (l0, l1d) => l1d := l0
  }

  l1d_nodes.zip(l2_nodes).zipWithIndex map {
    case ((l1d, l2), i) => l2 := 
        TLLogger(s"L2_L1[${i}].C[0]", !cacheParams.FPGAPlatform && cacheParams.enableTLLog) := 
        TLBuffer() := l1d
  }

  l2_nodes.zipWithIndex map {
    case(l2, i) => xbar := 
      TLLogger(s"L3_L2[${i}]", !cacheParams.FPGAPlatform && cacheParams.enableTLLog) := 
      TLBuffer() := l2
  }

  ram.node :=
    TLXbar() :=*
      TLFragmenter(32, 64) :=*
      TLCacheCork() :=*
      TLDelayer(delayFactor) :=*
      TLLogger(s"MEM_L3", !cacheParams.FPGAPlatform && cacheParams.enableTLLog) :=*
      l3.node :=* xbar

  lazy val module = new LazyModuleImp(this) with Formal {
    val timer = WireDefault(0.U(64.W))
    val logEnable = WireDefault(false.B)
    val clean = WireDefault(false.B)
    val dump = WireDefault(false.B)

    dontTouch(timer)
    dontTouch(logEnable)
    dontTouch(clean)
    dontTouch(dump)

    coupledL2AsL1.foreach {
      l1 => {
        l1.module.io.debugTopDown <> DontCare
        l1.module.io.hartId := DontCare
        l1.module.io.l2_tlb_req <> DontCare
      }
    }

    coupledL2.foreach {
      l2 => {
        l2.module.io.debugTopDown <> DontCare
        l2.module.io.hartId := DontCare
        l2.module.io.l2_tlb_req <> DontCare
      }
    }

    val verify_timer = RegInit(0.U(50.W))
    verify_timer := verify_timer + 1.U

    val offsetBits = 6
    val setBits = 7
    val tagBits = 11
    val bankBits = 0

    val addr_offsetBits = 1
    val addr_setBits = 1
    val addr_tagBits = 3
    val block_bytes = 2

    val io = IO(Vec(nrL2, new Bundle() {
      // Input signals for formal verification
      val inputAddr = Input(UInt(ram.node.in.head._2.bundle.addressBits.W))
      val inputNeedT = Input(Bool())
    }))

    def parseAddress(x: UInt): (UInt, UInt, UInt) = {
      val offset = x
      val set = offset >> (offsetBits + bankBits)
      val tag = set >> setBits
      (tag(tagBits - 1, 0), set(setBits - 1, 0), offset(offsetBits - 1, 0))
    }

    io.foreach {
      i => {
        val (tag, set, offset) = parseAddress(i.inputAddr)
        assume(tag < (1 << addr_tagBits).U)
        assume(set < (1 << addr_setBits).U)
        assume(offset < (1 << addr_offsetBits).U)
      }
    }

    coupledL2AsL1.zipWithIndex.foreach{
      case (node, i) =>
        node.module.io_inputAddr := io(0).inputAddr
        node.module.io_inputNeedT := io(0).inputNeedT
    }

    coupledL2(0).module.slices.head match {
      case tlSlice: L2Slice =>
        val dir_resetFinish = BoringUtils.bore(tlSlice.directory.resetFinish)
        assume(verify_timer < 200.U || dir_resetFinish)
    }

    coupledL2AsL1.foreach { l1 =>
      l1.module.slices.head match {
        case tlSlice: L2Slice =>
          val data = BoringUtils.bore(tlSlice.io.out.a.bits.data)
          assume(data < (1 << block_bytes).U)
          tlSlice.mshrCtl.mshrs.zipWithIndex.foreach {
            case (mshr, i) =>
              val MSHRStatus = BoringUtils.bore(mshr.io.status.valid)
              val allocStatus = BoringUtils.bore(mshr.io.alloc.valid)
              val channel = BoringUtils.bore(mshr.io.status.bits.channel)
              if (i >= 4)
                assume(!MSHRStatus && !allocStatus)
              else if (i == 3)
                assume(channel =/= 1.U)
          }
      }
    }

    coupledL2.foreach { l2 =>
      l2.module.slices.head match {
        case tlSlice: L2Slice =>
          val saved_data = RegInit(0.U(512.W))

          val c_opcode = BoringUtils.bore(tlSlice.io.in.c.bits.opcode)
          val c_addr = BoringUtils.bore(tlSlice.io.in.c.bits.address)
          val c_data = BoringUtils.bore(tlSlice.io.in.c.bits.data)

          when(c_opcode === Release && c_addr === 0.U) {
            saved_data := c_data
          }

          val d_opcode = BoringUtils.bore(tlSlice.io.out.d.bits.opcode)
          val d_addr = BoringUtils.bore(tlSlice.io.out.d.bits.echo.lift(L2AddrKey).getOrElse(0.U))
          val d_data = BoringUtils.bore(tlSlice.io.out.d.bits.data)
          when(d_opcode === ProbeAck && d_addr === 0.U) {
            assert(d_data === saved_data)
          }

          tlSlice.mshrCtl.mshrs.zipWithIndex.foreach {
            case (mshr, i) =>
              val MSHRStatus = BoringUtils.bore(mshr.io.status.valid)
              val allocStatus = BoringUtils.bore(mshr.io.alloc.valid)
              val channel = BoringUtils.bore(mshr.io.status.bits.channel)
              if (i >= 4)
                assume(!MSHRStatus && !allocStatus)
              else if (i == 3)
                assume(channel =/= 1.U)
          }
      }
    }
  }
}

object VerifyTop extends App {
  val config = baseConfig(1).alterPartial({
    case L2ParamKey => L2Param(
      clientCaches = Seq(L1Param(aliasBitsOpt = Some(2))),
    )
    case HCCacheParamsKey => HCCacheParameters(
      echoField = Seq(DirtyField())
    )
  })
  val top = DisableMonitors(p => LazyModule(new VerifyTop()(p)))(config)

  FileRegisters.writeOutputFile(
    "Verilog",
    "VerifyTop.sv",
    ChiselStage.emitSystemVerilog(top.module, 
                                  args = Array("--warn-conf", "id=4:s"),
                                  firtoolOpts = Array("--disable-annotation-unknown"))
  )
}