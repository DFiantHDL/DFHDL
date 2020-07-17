/*
 *     This file is part of DFiant.
 *
 *     DFiant is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU Lesser General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     any later version.
 *
 *     DFiant is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     GNU Lesser General Public License for more details.
 *
 *     You should have received a copy of the GNU Lesser General Public License
 *     along with DFiant.  If not, see <https://www.gnu.org/licenses/>.
 */

package RISCV

import DFiant._
import DFiant.sim._

@df class Proc(program : Program) extends DFDesign {
  private val pc      = DFBits[32] init program.imem.startAddress
  private val imem    = new IMem(program.imem)(pc)
  private val decoder = new Decoder(imem.instOut)
  private val regFile = new RegFile(decoder.instOut)
  private val execute = new Execute(regFile.instOut)
  private val dmem    = new DMem(program.dmem)(execute.instOut)
  regFile.writeBack(dmem.instOut)
  pc := dmem.instOut.pcNext

  ///////////////////////////////////////////////////////////////////////////////////////////////
  // Simulation Only
  ///////////////////////////////////////////////////////////////////////////////////////////////
  private val done = DFBit() <> OUT init false
  private val ppc =  pc.prev
  sim.report(msg"PC=$ppc, instRaw=${imem.instOut.raw}, debugOp=${decoder.instOut.debugOp}")

  program.imem.failAddress match {
    case Some(failPC) => ifdf(ppc === failPC){
      sim.report(msg"Test failed")
      sim.finish()
      done := true
    }
    case None =>
  }
  ifdf (ppc === program.imem.finishAddress) {
    sim.report(msg"Program execution finished")
    sim.finish()
    done := true
  }

  sim.assert(decoder.instOut.debugOp =!= DebugOp.Unsupported, msg"Unsupported instruction", severity = sim.Failure)
  ///////////////////////////////////////////////////////////////////////////////////////////////
}

@df class riscv_tb(program : Program) extends DFSimDesign {
  final val proc = new Proc(program)
  import compiler.sync._
  this !! ClockParams("clk", ClockParams.Edge.Rising)
  this !! ResetParams("rstn", ResetParams.Mode.Async, ResetParams.Active.Low)
}

object ProcSynth extends App {
  import compiler.backend.verilog.v2005
  final val proc = new Proc(Program.fromFile("riscv-bmarks/towers.riscv.dump"))
  proc
    .compile
    .printCodeString.printGenFiles()
    .toFolder("testProcSynth")

}

object ProcTest extends App {
  import compiler.backend.vhdl.v2008
  import sim.tools.ghdl
  val riscv_tb = new riscv_tb(Program.fromFile("riscv-bmarks/towers.riscv.dump"))
  riscv_tb
    .compile
    .printCodeString.printGenFiles()
    .toFolder("testProc")
    .simulation
    .run()


  //spike -l --isa=RV32IMAFDC towers.riscv 2>&1 >/dev/null | awk '{print $3}' | tr a-z A-Z | sed -e 's/0XFFFFFFFF//g'
  //ghdl -r -P/opt/ghdl/lib/ghdl/vendors/xilinx-vivado/ -frelaxed-rules --ieee=synopsys --std=08 riscv_tb --ieee-asserts=disable-at-0 --stop-time=5000ns | awk '{print $3}' | sed -e 's/PC=//g' | sed -e 's/,//g'
  //ghdl -r -frelaxed-rules --ieee=synopsys --std=08 riscv_tb --ieee-asserts=disable-at-0 | awk '{print $3}' | sed -e 's/PC=//g' | sed -e 's/,//g' > test.txt

}
