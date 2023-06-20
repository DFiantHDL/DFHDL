package dfhdl.tools
import toolsCore.*

object linters:
  given verilator: Linter = Verilator

object builders:
  given vivado: Builder = Vivado
