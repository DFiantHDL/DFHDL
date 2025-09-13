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

object OpenFPGALoader extends Programmer:
  val toolName: String = "openFPGALoader"
  protected def binExec: String =
    if (programIsAccessible("openFPGALoaderWSL")) "openFPGALoaderWSL"
    else "openFPGALoader"
  override protected def windowsBinExec: String =
    if (programIsAccessible("openFPGALoaderWSL")) "openFPGALoaderWSL.bat"
    else "openFPGALoader.exe"
  protected def versionCmd: String = s"-V"
  protected def extractVersion(cmdRetStr: String): Option[String] =
    val versionPattern = """openFPGALoader\s+v(\d+\.\d+\.\d+)""".r
    versionPattern.findFirstMatchIn(cmdRetStr).map(_.group(1))

  def program(
      cd: CompiledDesign
  )(using co: CompilerOptions, po: ProgrammerOptions): CompiledDesign =
    given MemberGetSet = cd.stagedDB.getSet
    val contraintCmdOption = cd.stagedDB.getSet.designDB.top.dclMeta.annotations.collectFirst {
      case annotation: constraints.ToolOptions => annotation.options.get(toolName)
    }.flatten
    val cmd = contraintCmdOption.getOrElse {
      val partName = cd.stagedDB.getSet.designDB.top.dclMeta.annotations.collectFirst {
        case annotation: constraints.DeviceID => annotation.deviceName
      }.getOrElse(throw new IllegalArgumentException("No device ID found"))
      s"--fpga-part $partName"
    }
    var probeFirmware: String = ""
    val suffix = (cd.vendor, po.flash) match
      case (Vendor.XilinxAMD | Vendor.Lattice, true)  => "mcs"
      case (Vendor.XilinxAMD | Vendor.Lattice, false) => "bit"
      case (Vendor.Gowin, false)                      => "fs"
      case (Vendor.AlteraIntel(pro), false)           =>
        val fullQuartusProgrammerPath =
          if (pro) QuartusProgrammerPro.runExecFullPath
          else QuartusProgrammer.runExecFullPath
        val location =
          Paths.get(fullQuartusProgrammerPath).getParent().resolve("blaster_6810.hex")
        probeFirmware = s" --probe-firmware $location"
        "svf"
      case x =>
        throw new IllegalArgumentException(
          s"Vendor-flash combination $x is currently not supported in this DFHDL openFPGALoader integration"
        )
    val fileName = s"${topName}.$suffix"
    exec(
      s"$cmd$probeFirmware $fileName"
    )
    cd
  end program
end OpenFPGALoader
