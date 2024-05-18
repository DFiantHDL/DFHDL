package dfhdl.tools
import toolsCore.*

object linters:
  given verilator: Verilator.type = Verilator
  given ghdl: GHDL.type = GHDL

object builders:
  val vivado: Builder = Vivado
