package dfhdl

import dfhdl.core.Design
import dfhdl.compiler.stages.{CompiledDesign, StagedDesign}
import dfhdl.tools.toolsCore.{Builder, Programmer}
import dfhdl.options.*
import dfhdl.backends
import dfhdl.compiler.ir
import dfhdl.tools.{builders, programmers}
import ir.constraints.DeviceID.Vendor
import dfhdl.tools.toolsCore.*
import dfhdl.app.AppMode
export compiler.stages.printCodeString

extension (dsn: Design)
  def compile(using CompilerOptions, PrinterOptions): CompiledDesign =
    StagedDesign(dsn).compile

extension (dsn: => Design)
  private inline def runApp(appMode: AppMode)(using
      ElaborationOptions,
      CompilerOptions,
      LinterOptions,
      SimulatorOptions,
      BuilderOptions,
      ProgrammerOptions,
      AppOptions
  ): Unit =
    // this is the behavior we want to trap the compilation information at the call site
    // (so that `appCompileTime` inside `DFApp` is the current application compile time
    // and not the DFHDL library compile time)
    @scala.annotation.nowarn(
      "msg=New anonymous class definition will be duplicated at each inline site"
    )
    val app = new dfhdl.app.ManualDFApp(dsn) {}
    app.runManual(appMode)
  end runApp
  inline def lint(using CompilerOptions, LinterOptions, AppOptions): Unit =
    runApp(AppMode.lint)
  inline def simulate(using CompilerOptions, SimulatorOptions, AppOptions): Unit =
    runApp(AppMode.simulate)
  inline def build(using CompilerOptions, BuilderOptions, AppOptions): Unit =
    runApp(AppMode.build)
  inline def program(using CompilerOptions, ProgrammerOptions, AppOptions): Unit =
    runApp(AppMode.program)
end extension

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
    case (Vendor.XilinxAMD, builders.vendor)        => Vivado
    case (Vendor.Gowin, builders.vendor)            => GowinDesigner
    case (Vendor.Lattice, builders.vendor)          => Diamond
    case (Vendor.AlteraIntel(pro), builders.vendor) => if (pro) QuartusPrimePro else QuartusPrime
    // the open-source flow is a single generic builder that dispatches on the device id
    case (Vendor.Gowin | Vendor.Lattice, builders.foss) => YosysNextPNR
    case (vendor, tool)     => throw new IllegalArgumentException(
        s"No $tool builder tool support for vendor $vendor"
      )

  protected[dfhdl] def programmer(using po: ProgrammerOptions): Programmer = (vendor, po.tool) match
    case (Vendor.XilinxAMD, programmers.vendor)        => Vivado
    case (Vendor.Gowin, programmers.vendor)            => GowinProgrammer
    case (Vendor.AlteraIntel(pro), programmers.vendor) =>
      if (pro) QuartusProgrammerPro else QuartusProgrammer
    case (_, programmers.foss) => OpenFPGALoader
    case (vendor, tool)        => throw new IllegalArgumentException(
        s"No $tool programmer tool support for vendor $vendor"
      )

  protected[dfhdl] def build(using CompilerOptions, BuilderOptions): CompiledDesign =
    builder.build(builder.buildPreprocess(cd))

  protected[dfhdl] def program(using CompilerOptions, ProgrammerOptions): CompiledDesign =
    programmer.program(programmer.programPreprocess(cd))
end extension
