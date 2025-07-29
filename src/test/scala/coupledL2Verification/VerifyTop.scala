package coupledL2Verification

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
import freechips.rocketchip.tilelink.TLMessages.{GrantData, ProbeAckData, ReleaseData}
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
      echoField = Seq(L2AddrField()),
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

//    assert(verify_timer < 1000.U)

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
        assume(verify_timer < 100.U || dir_resetFinish)
    }

    val data_p1 = RegInit(0.U(256.W))
    val data_p2 = RegInit(0.U(256.W))
    val valid = RegInit(false.B)

    coupledL2AsL1.foreach { l1d =>
      l1d.module.slices.head match {
        case tlSlice: L2Slice =>
          val sig = BoringUtils.bore(tlSlice.mshrCtl.mshrs.head.state.s_release)
          fvAssert(sig)
      }
    }

    coupledL2.foreach { l2 =>
      l2.module.slices.head match {
        case tlSlice: L2Slice =>

          val counter = RegInit(0.U(8.W))
          val a_valid = BoringUtils.bore(tlSlice.io.in.a.valid)
          val a_addr = BoringUtils.bore(tlSlice.io.in.a.bits.address)
          val a_ready = BoringUtils.bore(tlSlice.io.in.a.ready)

          val c_opcode = BoringUtils.bore(tlSlice.io.in.c.bits.opcode)
          val c_addr = BoringUtils.bore(tlSlice.io.in.c.bits.address)
          val c_data = BoringUtils.bore(tlSlice.io.in.c.bits.data)
          val c_valid = BoringUtils.bore(tlSlice.io.in.c.valid)
          val c_ready = BoringUtils.bore(tlSlice.io.in.c.ready)
          val c_flag = RegInit(false.B)

          when(a_valid && a_addr === 0.U && a_ready) {
            counter := counter + 1.U
          }
          fvAssert(!(a_valid && a_addr === 0.U && a_ready))
          fvAssert(!(c_valid && c_addr === 0.U && c_ready))
          fvAssert(!(c_valid && c_ready))
          fvAssert(!(a_valid && a_addr === 0.U && a_ready && counter === 1.U))

          when((c_opcode === ReleaseData || c_opcode === ProbeAckData) && c_addr === 0.U && c_valid) {
            valid := true.B
            when(c_flag) {
              c_flag := false.B
              data_p2 := c_data
            }.otherwise {
              c_flag := true.B
              data_p1 := c_data
            }
          }

          val d_opcode = BoringUtils.bore(tlSlice.io.in.d.bits.opcode)
          val d_addr = BoringUtils.bore(tlSlice.io.in.d.bits.echo.lift(L2AddrKey).getOrElse(0.U))
          val d_data = BoringUtils.bore(tlSlice.io.in.d.bits.data)
          val d_valid = BoringUtils.bore(tlSlice.io.in.d.valid)
          val d_flag = RegInit(false.B)

          when(d_opcode === GrantData && d_addr === 0.U && d_valid) {
            when(d_flag) {
              d_flag := false.B
              fvAssert(d_data === data_p2 || !valid)
            }.otherwise {
              d_flag := true.B
              fvAssert(d_data === data_p1 || !valid)
            }
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
    "VerifyTop.sv",
    ChiselStage.emitSystemVerilog(top.module, 
                                  args = Array("--warn-conf", "id=4:s"),
                                  firtoolOpts = Array("--disable-annotation-unknown"))
  )
}