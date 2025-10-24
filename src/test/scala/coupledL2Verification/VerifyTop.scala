package coupledL2Verification

import chisel3._
import circt.stage.ChiselStage
import chisel3.util._
import chisel3.util.experimental.BoringUtils
import chiselFv._
import coupledL2._
import coupledL2.tl2tl.{Slice => L2Slice, _}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tile.MaxHartIdBits
import freechips.rocketchip.tilelink._
import huancun._
import org.chipsalliance.cde.config._
import utility._
import messageGenerator.{MessageGeneratorParam, TLMessageGenerator}

import java.io.File


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

  // Replace previous TLCoupledL2AsL1 (complex prefetch based) with simplified TLMessageGenerator
  val msgGens = (0 until nrL2).map { i =>
    val genParams = MessageGeneratorParam(
      name = s"L1_$i",
      sets = 4,
      ways = 4,
      blockBytes = cacheParams.blockBytes,
      channelBytes = TLChannelBeatBytes(cacheParams.blockBytes),
      sourceIdRange = IdRange(0, 8),
      reqField = Seq(AliasField(2)),
      respKey = cacheParams.respKey
    )
    LazyModule(new TLMessageGenerator(genParams))
  }
  val l1d_nodes = msgGens.map(_.node)

  val coupledL2 = (0 until nrL2).map(i => LazyModule(new TL2TLCoupledL2()(baseConfig(1).alter((_, here, _) => {
    case L2ParamKey => L2Param(
      name = s"l2$i",
      clientCaches = Seq(L1Param(aliasBitsOpt = Some(2))),
      echoField = Seq(DirtyField()),
      hartId = i,
    )
    case huancun.BankBitsKey => 0
    case LogUtilsOptionsKey => LogUtilsOptions(
      false,
      here(L2ParamKey).enablePerf,
      here(L2ParamKey).FPGAPlatform
    )
    case PerfCounterOptionsKey => PerfCounterOptions(
      here(L2ParamKey).enablePerf && !here(L2ParamKey).FPGAPlatform,
      here(L2ParamKey).enableRollingDB && !here(L2ParamKey).FPGAPlatform,
      XSPerfLevel.withName("VERBOSE"),
      i
    )
  }))))
  val l2_nodes = coupledL2.map(_.node)

  val l3 = LazyModule(new HuanCun()(baseConfig(1).alter((_, here, _) => {
    case HCCacheParamsKey => HCCacheParameters(
      name = "L3",
      level = 3,
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
    case LogUtilsOptionsKey => LogUtilsOptions(
      here(HCCacheParamsKey).enableDebug,
      here(HCCacheParamsKey).enablePerf,
      here(HCCacheParamsKey).FPGAPlatform
    )
    case PerfCounterOptionsKey => PerfCounterOptions(
      here(HCCacheParamsKey).enablePerf && !here(HCCacheParamsKey).FPGAPlatform,
      false,
      XSPerfLevel.withName("VERBOSE"),
      0
    )
  })))

  val xbar = TLXbar()
  val ram = LazyModule(new TLRAM(AddressSet(0, 0xff_ffffL), beatBytes = 32))

  l1d_nodes.zip(l2_nodes).zipWithIndex foreach { case ((l1d, l2), i) =>
    l2 := TLLogger(s"L2_L1[${i}].C[0]", !cacheParams.FPGAPlatform && cacheParams.enableTLLog) := TLBuffer() := l1d
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
    coupledL2.foreach {
      l2 => {
        l2.module.io.debugTopDown <> DontCare
        l2.module.io.hartId := DontCare
        l2.module.io.pfCtrlFromCore := DontCare
        l2.module.io.l2_tlb_req <> DontCare
      }
    }

    val verify_timer = RegInit(0.U(50.W))
    verify_timer := verify_timer + 1.U

    val io = IO(Vec(nrL2, new Bundle() {
      // External control to drive simplified generator
      val reqAddr = Input(UInt(ram.node.in.head._2.bundle.addressBits.W))
      val reqIsAcquire = Input(Bool())
      val reqParam = Input(Bool())
    }))

    msgGens.zipWithIndex.foreach { case (gen, i) =>
      gen.module.io_in_addr := io(i).reqAddr
      gen.module.io_in_isAcquire := io(i).reqIsAcquire
      gen.module.io_in_param := io(i).reqParam
    }

    coupledL2(0).module.slices.head match {
      case tlSlice: L2Slice =>
        val dir_resetFinish = BoringUtils.bore(tlSlice.directory.resetFinish)
        assume(verify_timer < 200.U || dir_resetFinish)
    }

    val timer = 500
    coupledL2.foreach { l2 =>
      l2.module.slices.head match {
        case tlSlice: L2Slice =>
          val w_rprobeacklast = BoringUtils.bore(tlSlice.mshrCtl.mshrs.head.state.w_rprobeacklast)
          astRelaxedLiveness(!w_rprobeacklast, w_rprobeacklast, timer)
          val w_pprobeacklast = BoringUtils.bore(tlSlice.mshrCtl.mshrs.head.state.w_pprobeacklast)
          astRelaxedLiveness(!w_pprobeacklast, w_pprobeacklast, timer)
          val w_grantlast = BoringUtils.bore(tlSlice.mshrCtl.mshrs.head.state.w_grantlast)
          astRelaxedLiveness(!w_grantlast, w_grantlast, timer)
          val w_releaseack = BoringUtils.bore(tlSlice.mshrCtl.mshrs.head.state.w_releaseack)
          astRelaxedLiveness(!w_releaseack, w_releaseack, timer)
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
  val directory = new File("./Verilog")

  if (!directory.exists()) {
    directory.mkdirs()
  }
  FileRegisters.writeOutputFile(
    "Verilog",
    "VerifyTop.sv",
    ChiselStage.emitSystemVerilog(top.module,
                                  args = Array("--warn-conf", "id=4:s"),
                                  firtoolOpts = Array("--disable-annotation-unknown"))
  )
}