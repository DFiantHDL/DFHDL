package dfhdl.tools.toolsCore

import dfhdl.core.Design
import dfhdl.compiler.stages.CompiledDesign
import dfhdl.compiler.ir.*
import dfhdl.internals.*
import dfhdl.options.*
import dfhdl.compiler.printing.Printer
import dfhdl.compiler.analysis.*
import java.nio.file.Paths
import dfhdl.backends
import dfhdl.vendor
import constraints.DeviceID.Vendor
import java.io.File.separatorChar

object QuartusProgrammer extends Programmer:
  val toolName: String = "quartus"
  protected def binExec: String = "quartus_pgm"
  protected def versionCmd: String = "-v"
  protected def extractVersion(cmdRetStr: String): Option[String] =
    val versionPattern = """(?s)Quartus.*Version (\d+\.\d+)""".r
    versionPattern.findFirstMatchIn(cmdRetStr).map(_.group(1))

  def program(
      cd: CompiledDesign
  )(using co: CompilerOptions, po: ProgrammerOptions): CompiledDesign =
    given MemberGetSet = cd.stagedDB.getSet
    val contraintCmdOption = cd.stagedDB.getSet.designDB.top.dclMeta.annotations.collectFirst {
      case annotation: constraints.ToolOptions => annotation.options.get(toolName)
    }.flatten
    val deviceName = cd.stagedDB.getSet.designDB.top.dclMeta.annotations.collectFirst {
      case annotation: constraints.DeviceID => annotation.deviceName
    }.getOrElse(throw new IllegalArgumentException("No device ID found"))
    // file path needs to be absolute for programmer_cli
    val fileName = s"${topName}.sof"
    val cmd = s"""-m jtag -c 1 -o "p;$fileName""""
    exec(
      cmd
    )
    cd
  end program
end QuartusProgrammer
