/** *************************************************************************************
  * Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
  * Copyright (c) 2020-2021 Peng Cheng Laboratory
  *
  * XiangShan is licensed under Mulan PSL v2.
  * You can use this software according to the terms and conditions of the Mulan PSL v2.
  * You may obtain a copy of Mulan PSL v2 at:
  *          http://license.coscl.org.cn/MulanPSL2
  *
  * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
  * EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
  * MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
  *
  * See the Mulan PSL v2 for more details.
  * *************************************************************************************
  */

package coupledL2AsL1

import chisel3._
import coupledL2._
import freechips.rocketchip.diplomacy._
import org.chipsalliance.cde.config.Parameters

class TLCoupledL2AsL1(implicit p: Parameters) extends CoupledL2Base {
  println(s"prefetchers: $prefetchers")
  assert(prefetchers.length == 1 && prefetchers.exists(_.isInstanceOf[InputAsPrefectchParam]))

  class CoupledL2AsL1Imp(wrapper: LazyModule) extends BaseCoupledL2Imp(wrapper) {
    override lazy val prefetcher = prefetchOpt.map(_ => Module(new Input2Req()(pftParams)))
    val fullAddrBits = node.in.head._2.bundle.addressBits

    // keep io_name same as before
    val io_inputAddr = IO(Input(UInt(fullAddrBits.W)))
    val io_inputNeedT = IO(Input(Bool()))

    prefetchOpt.foreach {
       _ =>
        prefetcher.get.io_inputAddr := io_inputAddr
        prefetcher.get.io_inputNeedT := io_inputNeedT
    }
  }

  lazy val module = new CoupledL2AsL1Imp(this)
}