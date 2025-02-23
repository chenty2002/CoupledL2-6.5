package chiselFv

import chisel3.experimental.SourceInfo
import chisel3._
import chisel3.ltl.{AssertProperty, Sequence}


trait Formal {
  this: Module => 
  
  private val resetCounter = Module(new ResetCounter)
  resetCounter.io.clk := this.clock
  resetCounter.io.reset := this.reset
  val timeSinceReset = resetCounter.io.timeSinceReset
  val notChaos = resetCounter.io.notChaos


  def fvAssert(cond: Bool, msg: String = "")
              (implicit sourceInfo: SourceInfo): Unit = {
    when(notChaos) {
      assert(cond, msg)
    }
  }

  def assertAt(n: UInt, cond: Bool, msg: String = "")
              (implicit sourceInfo: SourceInfo): Unit = {
    when(notChaos && timeSinceReset === n) {
      assert(cond, msg)
    }
  }

  def assertAfterNStepWhen(cond: Bool, n: Int, asert: Bool, msg: String = "")
                          (implicit sourceInfo: SourceInfo): Unit = {
    val next = RegInit(VecInit(Seq.fill(n)(false.B)))
    when(cond && notChaos) {
      next(0) := true.B
    }.otherwise {
      next(0) := false.B
    }
    for (i <- 1 until n) {
      next(i) := next(i - 1)
    }
    when(next(n - 1)) {
      fvAssert(asert, msg)
    }
  }

  def assertNextStepWhen(cond: Bool, asert: Bool, msg: String = "")
                        (implicit sourceInfo: SourceInfo): Unit = {
    assertAfterNStepWhen(cond, 1, asert, msg)
  }

  def assertAlwaysAfterNStepWhen(cond: Bool, n: Int, asert: Bool, msg: String = "")
                                (implicit sourceInfo: SourceInfo): Unit = {
    val next = RegInit(VecInit(Seq.fill(n)(false.B)))
    when(cond && notChaos) {
      next(0) := true.B
    }
    for (i <- 1 until n) {
      next(i) := next(i - 1)
    }
    when(next(n - 1)) {
      fvAssert(asert, msg)
    }
  }

  def past[T <: Data](value: T, n: Int)(block: T => Any)
                     (implicit sourceInfo: SourceInfo): Unit = {
    when(notChaos && timeSinceReset >= n.U) {
      block(Delay(value, n))
    }
  }

  def initialReg(w: Int, v: Int): InitialReg = {
    val reg = Module(new InitialReg(w, v))
    reg.io.clk := clock
    reg.io.reset := reset
    reg
  }

  def anyconst(w: Int): UInt = {
    val cst = Module(new AnyConst(w))
    cst.io.out
  }

  def astLiveness(req: Bool, resp: Bool, msg: String = "")(implicit sourceInfo: SourceInfo): Unit = {
    val reqProp: Sequence = req
    val respProp: Sequence = resp
    when(notChaos) {
      AssertProperty(reqProp |-> respProp.eventually, label = Option(msg))
    }
  }

  def astRelaxedLiveness(req: Bool, resp: Bool, n: Int, msg: String = "")
                        (implicit sourceInfo: SourceInfo): Unit = {
    val reqProp: Sequence = req
    val respProp: Sequence = resp
    when(notChaos) {
      AssertProperty(reqProp |-> respProp.delayRange(1, n), label = Option(msg))
    }
  }

  def assertLivenessTimer(cond: Bool, reset: Bool, n: Int, msg: String = "")
                         (implicit sourceInfo: SourceInfo): Unit = {
    val timer = RegInit(0.U(64.W))
    when(reset) {
      timer := 1.U
    }.elsewhen(cond) {
      timer := timer + 1.U
    }
    when(notChaos) {
      assert(timer <= n.U, msg)
    }
  }
}
