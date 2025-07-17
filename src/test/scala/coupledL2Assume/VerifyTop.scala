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
import freechips.rocketchip.tilelink._
import huancun._
import org.chipsalliance.cde.config._
import utility._

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
      ways = 2,
      sets = 2,
      blockBytes = 2,
      channelBytes = TLChannelBeatBytes(1),
      mshrs = 4,
      clientCaches = Seq(L1Param(aliasBitsOpt = Some(2))),
      // echoField = Seq(DirtyField()),
      hartId = i,
      prefetch = Seq(InputAsPrefectchParam())
    )
    case huancun.BankBitsKey => 0 // FV: 1 bank for L1s
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
  })))
  )
  val l1d_nodes = coupledL2AsL1.map(_.node)

  val coupledL2 = (0 until nrL2).map(i => LazyModule(new TL2TLCoupledL2()(baseConfig(1).alter((_, here, _) => {
    case L2ParamKey => L2Param(
      name = s"l2$i",
      ways = 2,
      sets = 4,
      blockBytes = 2,
      channelBytes = TLChannelBeatBytes(1),
      mshrs = 4,
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
      ways = 2,
      sets = 4,
      blockBytes = 2,
      channelBytes = TLChannelBeatBytes(1),
      mshrs = 6,
      inclusive = false,
      clientCaches = (0 until nrL2).map(_ =>
        CacheParameters(
          name = s"l2",
          sets = 4,
          ways = 2 + 2,
          blockGranularity = log2Ceil(4)
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
  val ram = LazyModule(new TLRAM(AddressSet(0, 0x1fL), beatBytes = 1))

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
      TLFragmenter(1, 2) :=*
      TLCacheCork() :=*
      TLDelayer(delayFactor) :=*
      TLLogger(s"MEM_L3", !cacheParams.FPGAPlatform && cacheParams.enableTLLog) :=*
      l3.node :=* xbar

  lazy val module = new LazyModuleImp(this) with Formal {
    coupledL2AsL1.foreach {
      l1 => {
        l1.module.io.debugTopDown <> DontCare
        l1.module.io.hartId := DontCare
        l1.module.io.pfCtrlFromCore := DontCare
        l1.module.io.l2_tlb_req <> DontCare
      }
    }

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
      // Input signals for formal verification
      val inputAddr = Input(UInt(ram.node.in.head._2.bundle.addressBits.W))
      val inputNeedT = Input(Bool())
    }))

    coupledL2AsL1.zipWithIndex.foreach{
      case (node, i) =>
        node.module.io_inputAddr := io(i).inputAddr
        node.module.io_inputNeedT := io(i).inputNeedT
    }

    coupledL2(0).module.slices.head match {
      case tlSlice: L2Slice =>
        val dir_resetFinish = BoringUtils.bore(tlSlice.directory.resetFinish)
        assume(verify_timer < 200.U || dir_resetFinish)
    }

    val timer = 500
    val sw_verification_flag = 1
    coupledL2.foreach { l2 =>
      l2.module.slices.head match {
        case tlSlice: L2Slice =>
          if(sw_verification_flag == 0) {
//            val s_acquire = BoringUtils.bore(tlSlice.mshrCtl.mshrs.head.state.s_acquire)
//            astRelaxedLiveness(!s_acquire, s_acquire, timer)
//            val s_rprobe = BoringUtils.bore(tlSlice.mshrCtl.mshrs.head.state.s_rprobe)
//            astRelaxedLiveness(!s_rprobe, s_rprobe, timer)
//            val s_pprobe = BoringUtils.bore(tlSlice.mshrCtl.mshrs.head.state.s_pprobe)
//            astRelaxedLiveness(!s_pprobe, s_pprobe, timer)
//            val s_release = BoringUtils.bore(tlSlice.mshrCtl.mshrs.head.state.s_release)
//            astRelaxedLiveness(!s_release, s_release, timer)
//            val s_probeack = BoringUtils.bore(tlSlice.mshrCtl.mshrs.head.state.s_probeack)
//            astRelaxedLiveness(!s_probeack, s_probeack, timer)
//            val s_refill = BoringUtils.bore(tlSlice.mshrCtl.mshrs.head.state.s_refill)
//            astRelaxedLiveness(!s_refill, s_refill, timer)
//            val s_retry = BoringUtils.bore(tlSlice.mshrCtl.mshrs.head.state.s_retry)
//            astRelaxedLiveness(!s_retry, s_retry, timer)
          } else {
//            val w_rprobeackfirst = BoringUtils.bore(tlSlice.mshrCtl.mshrs.head.state.w_rprobeackfirst)
//            astRelaxedLiveness(!w_rprobeackfirst, w_rprobeackfirst, timer)
            val w_rprobeacklast = BoringUtils.bore(tlSlice.mshrCtl.mshrs.head.state.w_rprobeacklast)
            astRelaxedLiveness(!w_rprobeacklast, w_rprobeacklast, timer)
//            val w_pprobeackfirst = BoringUtils.bore(tlSlice.mshrCtl.mshrs.head.state.w_pprobeackfirst)
//            astRelaxedLiveness(!w_pprobeackfirst, w_pprobeackfirst, timer)
            val w_pprobeacklast = BoringUtils.bore(tlSlice.mshrCtl.mshrs.head.state.w_pprobeacklast)
            astRelaxedLiveness(!w_pprobeacklast, w_pprobeacklast, timer)
//            val w_grantfirst = BoringUtils.bore(tlSlice.mshrCtl.mshrs.head.state.w_grantfirst)
//            astRelaxedLiveness(!w_grantfirst, w_grantfirst, timer)
            val w_grantlast = BoringUtils.bore(tlSlice.mshrCtl.mshrs.head.state.w_grantlast)
            astRelaxedLiveness(!w_grantlast, w_grantlast, timer)
//            val w_grant = BoringUtils.bore(tlSlice.mshrCtl.mshrs.head.state.w_grant)
//            astRelaxedLiveness(!w_grant, w_grant, timer)
            val w_releaseack = BoringUtils.bore(tlSlice.mshrCtl.mshrs.head.state.w_releaseack)
            astRelaxedLiveness(!w_releaseack, w_releaseack, timer)
//            val w_replResp = BoringUtils.bore(tlSlice.mshrCtl.mshrs.head.state.w_replResp)
//            astRelaxedLiveness(!w_replResp, w_replResp, timer)
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
  val directory = new File("./Verilog")

  if (!directory.exists()) {
    directory.mkdirs()
  }
  FileRegisters.writeOutputFile(
    "Verilog",
    "VerifyTop_w.sv",
    ChiselStage.emitSystemVerilog(top.module,
                                  args = Array("--warn-conf", "id=4:s"),
                                  firtoolOpts = Array("--disable-annotation-unknown"))
  )
}