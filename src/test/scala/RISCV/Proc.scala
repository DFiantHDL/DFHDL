package RISCV
import DFiant._

class Proc(programMem : ProgramMem)(implicit ctx : DFDesign.ContextOf[Proc]) extends DFDesign {
  private val pc = DFBits[32] init StartAddress
  pc.keep

  private val imem = new IMem(programMem)(pc)
  private val decoder = new Decoder(imem.inst)
  private val regFile = new RegFile(decoder.inst)
  private val execute = new Execute(regFile.inst)
  private val dmem = new DMem(execute.inst)
  regFile.writeBack(dmem.inst)

  pc := dmem.inst.pcNext
}

class Proc_TB(programMem : ProgramMem)(implicit ctx : DFDesign.ContextOf[Proc_TB]) extends DFSimulator {
  val proc = new Proc(programMem)
}

object ProcTest extends App {
//  val riscv = new Proc {}.compileToVHDL.print().toFile("test.vhd")
  val riscv_tb = new Proc_TB(ProgramMem.fromFile("testA.asm")).compileToVHDL.print().toFile("test.vhd")
  val libraryLocation = s"c:/ghdl/lib/vendors/xilinx-vivado"
  val flags = s"-P$libraryLocation -frelaxed-rules --ieee=synopsys --std=08"
  import sys.process._
  import scala.language.postfixOps
  {s"ghdl -a $flags test.vhd" !!}
  {s"ghdl -r $flags riscv_tb --ieee-asserts=disable-at-0 --stop-time=100ns" !}

}