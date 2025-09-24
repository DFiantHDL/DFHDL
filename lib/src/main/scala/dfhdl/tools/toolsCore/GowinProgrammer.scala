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

object GowinProgrammer extends Programmer:
  val toolName: String = "gowin"
  protected def binExec: String = "programmer_cli"
  // tool does not have a version command and it will be extracted from installed path
  protected def versionCmd: String = ""
  protected def extractVersion(cmdRetStr: String): Option[String] =
    val versionPattern = """.*Gowin_V(\d+\.\d+\.\d+\.\d+)""".r
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
    val fileName = s"${Paths.get(execPath).toAbsolutePath()}$separatorChar${topName}.fs"
    val runMode = if (po.flash) 53 else 2
    val cmd = s"--device $deviceName --run $runMode --fsFile $fileName"
    exec(
      cmd
    )
    cd
  end program
end GowinProgrammer
