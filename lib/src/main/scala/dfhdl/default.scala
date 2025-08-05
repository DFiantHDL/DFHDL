package dfhdl

import dfhdl.core.Design
import dfhdl.compiler.stages.CompiledDesign
import dfhdl.tools.toolsCore.{Builder, Programmer}
import dfhdl.options.*
import dfhdl.backends

extension (cd: CompiledDesign)
  def lint(using
      co: CompilerOptions,
      lo: LinterOptions
  ): CompiledDesign =
    co.backend match
      case _: backends.verilog => lo.verilogLinter.lint(lo.verilogLinter.lintPreprocess(cd))
      case _: backends.vhdl    => lo.vhdlLinter.lint(lo.vhdlLinter.lintPreprocess(cd))

  def simPrep(using
      co: CompilerOptions,
      so: SimulatorOptions
  ): CompiledDesign =
    val stagedDB = cd.stagedDB
    import stagedDB.getSet
    if (stagedDB.inSimulation) co.backend match
      case _: backends.verilog => so.verilogSimulator.simulatePreprocess(cd)
      case _: backends.vhdl    => so.vhdlSimulator.simulatePreprocess(cd)
    else throw new Exception(
      s"The top design `${stagedDB.top.getName}` has ports and therefore cannot be simulated."
    )
  end simPrep

  def simRun(using
      co: CompilerOptions,
      so: SimulatorOptions
  ): CompiledDesign = co.backend match
    case _: backends.verilog => so.verilogSimulator.simulate(cd)
    case _: backends.vhdl    => so.vhdlSimulator.simulate(cd)

  def simulate(using
      co: CompilerOptions,
      so: SimulatorOptions
  ): CompiledDesign = simPrep.simRun

  def build(using builder: Builder)(using CompilerOptions, BuilderOptions): CompiledDesign =
    builder.build(builder.buildPreprocess(cd))

  def program(using
      programmer: Programmer
  )(using CompilerOptions, ProgrammerOptions): CompiledDesign =
    programmer.program(programmer.programPreprocess(cd))
end extension
