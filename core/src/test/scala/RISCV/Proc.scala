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

class Proc(program : Program)(implicit ctx : DFDesign.ContextOf[Proc]) extends DFDesign {
  private val pc = DFBits[32] init program.imem.startAddress
  private val imem = new IMem(program.imem)(pc)
  private val decoder = new Decoder(imem.inst)
  private val regFile = new RegFile(decoder.inst)
  private val execute = new Execute(regFile.inst)
  private val dmem = new DMem(program.dmem)(execute.inst)
  regFile.writeBack(dmem.inst)






  ///////////////////////////////////////////////////////////////////////////////////////////////
  // Simulation Only
  ///////////////////////////////////////////////////////////////////////////////////////////////
  private val done = DFBool() <> OUT init false
  private val ppc =  pc.prev
  sim.report(msg"PC=$ppc, instRaw=${imem.inst.instRaw}, debugOp=${decoder.inst.debugOp}")

  program.imem.failAddress match {
    case Some(failPC) => ifdf(ppc == failPC){
      sim.report(msg"Test failed")
      sim.finish()
      done := true
    }
    case None =>
  }
  ifdf (ppc == program.imem.finishAddress) {
    sim.report(msg"Program execution finished")
    sim.finish()
    done := true
  }

  sim.assert(decoder.inst.debugOp != DebugOp.Unsupported, msg"Unsupported instruction", severity = Severity.Failure)
  ///////////////////////////////////////////////////////////////////////////////////////////////

  pc := dmem.inst.pcNext
}

class Proc_TB(program : Program)(implicit ctx : DFDesign.ContextOf[Proc_TB]) extends DFSimulator {
  val proc = new Proc(program)
}

object ProcTest extends DFApp {
//  val riscv = new Proc {}.compileToVHDL.print().toFile("test.vhd")
  val riscv_tb = new Proc_TB(Program.fromFile("../riscv-bmarks/towers.riscv.dump")).compileToVHDL.print().toFolder("testProc")
  new java.io.File("testProc/work").mkdirs()
  val workDirFlag = "--workdir=testProc/work"
  val libraryLocation = s"/opt/ghdl/lib/ghdl/vendors/xilinx-vivado/"
    val flags = s"$workDirFlag -P$libraryLocation -frelaxed-rules --ieee=synopsys --std=08"
  import sys.process._
  import scala.language.postfixOps

  {s"ghdl --clean $workDirFlag" !!}
  {s"ghdl -a $flags ${riscv_tb.getFiles}" !!}
  {s"ghdl -r $flags riscv_tb --ieee-asserts=disable-at-0" !}
  //spike -l --isa=RV32IMAFDC towers.riscv 2>&1 >/dev/null | awk '{print $3}' | tr a-z A-Z | sed -e 's/0XFFFFFFFF//g'
  //ghdl -r -P/opt/ghdl/lib/ghdl/vendors/xilinx-vivado/ -frelaxed-rules --ieee=synopsys --std=08 riscv_tb --ieee-asserts=disable-at-0 --stop-time=5000ns | awk '{print $3}' | sed -e 's/PC=//g' | sed -e 's/,//g'
  //ghdl -r -frelaxed-rules --ieee=synopsys --std=08 riscv_tb --ieee-asserts=disable-at-0 | awk '{print $3}' | sed -e 's/PC=//g' | sed -e 's/,//g' > test.txt

}
