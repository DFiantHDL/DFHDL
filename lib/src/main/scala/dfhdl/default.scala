package dfhdl

import dfhdl.core.Design
import dfhdl.compiler.stages.CompiledDesign
import dfhdl.tools.toolsCore.{Builder, Programmer}
import dfhdl.options.*
import dfhdl.backends
import dfhdl.compiler.ir
import dfhdl.tools.{builders, programmers}
import ir.constraints.DeviceID.Vendor
import dfhdl.tools.toolsCore.*

extension (cd: CompiledDesign)
  def lint(using
      co: CompilerOptions,
      lo: LinterOptions
  ): CompiledDesign =
    co.backend match
      case _: backends.verilog => lo.verilogLinter.lint(lo.verilogLinter.lintPreprocess(cd))
      case _: backends.vhdl    => lo.vhdlLinter.lint(lo.vhdlLinter.lintPreprocess(cd))

  protected[dfhdl] def simPrep(using
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

  protected[dfhdl] def simRun(using
      co: CompilerOptions,
      so: SimulatorOptions
  ): CompiledDesign = co.backend match
    case _: backends.verilog => so.verilogSimulator.simulate(cd)
    case _: backends.vhdl    => so.vhdlSimulator.simulate(cd)

  protected[dfhdl] def simulate(using
      co: CompilerOptions,
      so: SimulatorOptions
  ): CompiledDesign = simPrep.simRun

  protected[dfhdl] def vendor = cd.stagedDB.top.dclMeta.annotations.collectFirst {
    case annotation: ir.constraints.DeviceID => annotation.vendor
  }.getOrElse(throw new IllegalArgumentException("No device constraint found"))

  protected[dfhdl] def builder(using bo: BuilderOptions): Builder = (vendor, bo.tool) match
    case (Vendor.XilinxAMD, builders.vendor)   => Vivado
    case (Vendor.Gowin, builders.vendor)       => GowinDesigner
    case (Vendor.Lattice, builders.vendor)     => Diamond
    case (Vendor.AlteraIntel, builders.vendor) => QuartusPrime
    case (vendor, tool)                        => throw new IllegalArgumentException(
        s"No $tool builder tool support for vendor $vendor"
      )

  protected[dfhdl] def programmer(using po: ProgrammerOptions): Programmer = (vendor, po.tool) match
    case (Vendor.XilinxAMD, programmers.vendor)   => Vivado
    case (Vendor.Gowin, programmers.vendor)       => GowinProgrammer
    case (Vendor.AlteraIntel, programmers.vendor) => QuartusProgrammer
    case (_, programmers.foss)                    => OpenFPGALoader
    case (vendor, tool)                           => throw new IllegalArgumentException(
        s"No $tool programmer tool support for vendor $vendor"
      )

  protected[dfhdl] def build(using CompilerOptions, BuilderOptions): CompiledDesign =
    builder.build(builder.buildPreprocess(cd))

  protected[dfhdl] def program(using CompilerOptions, ProgrammerOptions): CompiledDesign =
    programmer.program(programmer.programPreprocess(cd))
end extension
