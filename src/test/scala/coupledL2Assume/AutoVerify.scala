package coupledL2Assume

import circt.stage.ChiselStage
import coupledL2._
import freechips.rocketchip.diplomacy.{DisableMonitors, LazyModule}
import huancun.{DirtyField, HCCacheParameters, HCCacheParamsKey}
import utility._

import java.io._
import scala.collection.mutable.ArrayBuffer
import scala.sys.process._


object AutoVerify extends App {
  def modifyPy(filename: String): Unit = {
    val pylines = new ArrayBuffer[String]()
    val pyFile = new BufferedReader(new FileReader(new File("set_verify.py")))
    var line = pyFile.readLine()
    while(line != null) {
      pylines.append(
        line.replaceFirst("open\\('.*'\\) as fin:", "open('Verilog/VerifyTop.sv') as fin:")
          .replaceFirst("open\\('.*', 'w'\\) as fout:", s"open('${filename}', 'w') as fout:")
      )
      line = pyFile.readLine()
    }
    pyFile.close()

    val newPy = new BufferedWriter(new FileWriter(new File("set_verify.py")))
    pylines.foreach { line =>
      newPy.write(line + "\n")
    }
    newPy.close()
  }

  val config = baseConfig(1).alterPartial({
    case L2ParamKey => L2Param(
      clientCaches = Seq(L1Param(aliasBitsOpt = Some(2))),
    )
    case HCCacheParamsKey => HCCacheParameters(
      echoField = Seq(DirtyField())
    )
  })

  val suffix = "performance-300-500-1000"
  val path = "/home/lyj238/VerifyL2"
  val top = DisableMonitors(p => LazyModule(new VerifyTop()(p)))(config)

  FileRegisters.writeOutputFile(
    "Verilog",
    "VerifyTop.sv",
    ChiselStage.emitSystemVerilog(top.module, 
                                  args = Array("--warn-conf", "id=4:s"),
                                  firtoolOpts = Array("--disable-annotation-unknown"))
  )
  //  val cp = s"cp Verilog/VerifyTop.sv .".!
  val filename = s"VerifyTop_${suffix}.sv"
  modifyPy(filename)
  val py = "python set_verify.py".!
  println(s"Verilog File Name: ${filename}")
  val server_addr = "lyj238@192.168.20.110"
  val server_path = "/home/lyj238/VerifyL2/Chisel6.5/mshrctl_assume"
  val scp_cmd = s"scp ${filename} ${server_addr}:${server_path}/${filename}".!
  if(scp_cmd == 0) {
    println("scp success")
  } else {
    println("scp failure")
  }
  //  val rm = s"rm -f ${path}/${suffix}/${filename}".!
  //  val cpjg = s"cp ${filename} ${path}/${suffix}".!
}
