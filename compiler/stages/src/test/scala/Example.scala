import dfhdl.*
import dfhdl.compiler.stages.verilog.*

class ALU extends EDDesign:
  // cell coordinates
  val read_step = UInt(8) <> VAR // reading step

  // life generation state
  process(all) {
    read_step match
      case 0 =>
      case 1 =>
        // 1 cycle to set address and 1 cycle BRAM read latency
        read_step match
          case _ =>
  }
end ALU

@main def hello: Unit =
  import backends.verilog.sv2005
  val top = new ALU
  import compiler.stages.StageRunner
  StageRunner.logDebug()
  top.compile
