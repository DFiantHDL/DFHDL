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

  pc := dmem.inst.pcNext
}

class Proc_TB(program : Program)(implicit ctx : DFDesign.ContextOf[Proc_TB]) extends DFSimulator {
  val proc = new Proc(program)
}

object ProcTest extends App {
//  val riscv = new Proc {}.compileToVHDL.print().toFile("test.vhd")
  val riscv_tb = new Proc_TB(Program.fromFile("towers.riscv.dump")).compileToVHDL.print().toFile("test.vhd")
  val libraryLocation = s"/opt/ghdl/lib/ghdl/vendors/xilinx-vivado/"
    val flags = s"-P$libraryLocation -frelaxed-rules --ieee=synopsys --std=08"
  import sys.process._
  import scala.language.postfixOps
  {s"ghdl -a $flags test.vhd" !!}
  {s"ghdl -r $flags riscv_tb --ieee-asserts=disable-at-0 --stop-time=150000ns" !}
  //spike -l --isa=RV32IMAFDC towers.riscv 2>&1 >/dev/null | awk '{print $3}' | tr a-z A-Z | sed -e 's/0XFFFFFFFF//g'
  //ghdl -r -P/opt/ghdl/lib/ghdl/vendors/xilinx-vivado/ -frelaxed-rules --ieee=synopsys --std=08 riscv_tb --ieee-asserts=disable-at-0 --stop-time=5000ns | awk '{print $3}' | sed -e 's/PC=//g' | sed -e 's/,//g'

}
