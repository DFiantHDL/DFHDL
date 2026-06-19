package dfhdl.tools.toolsCore

import dfhdl.core.Design
import dfhdl.compiler.stages.CompiledDesign
import dfhdl.compiler.ir.*
import dfhdl.internals.*
import dfhdl.options.*
import dfhdl.compiler.analysis.*
import java.nio.file.Paths
import dfhdl.backends
import dfhdl.compiler.stages.verilog.VerilogDialect
import dfhdl.compiler.stages.vhdl.VHDLDialect
import dfhdl.compiler.ir.constraints
import constraints.DeviceID
import DeviceID.Vendor
import dfhdl.compiler.ir.PhysicalNumber.Ops.MHz

/** A single, generic open-source bitstream build flow built around the OSS CAD Suite toolchain
  * (`yosys` + `nextpnr-*` + an architecture-specific bitstream packer).
  *
  * The concrete flow is selected at build time from the design's [[constraints.DeviceID]], so one
  * builder serves every device family that the open-source tools can target. Supported today:
  *   - Lattice ECP5: `synth_ecp5` -> `nextpnr-ecp5` -> `ecppack` (Project Trellis), `.lpf`
  *     constraints, produces `<top>.bit`.
  *   - Gowin GW1N/GW2A: `synth_gowin` -> `nextpnr-himbaechel` -> `gowin_pack` (Project Apicula),
  *     `.cst` constraints, produces `<top>.fs`.
  *
  * Adding another supported device only requires extending [[archFor]] with a new [[Arch]]; no
  * changes are needed in the tool-selection logic. The whole OSS CAD Suite ships in a single `bin`
  * directory, so `yosys` is the primary executable (for availability/version detection) and the
  * downstream tools are resolved as siblings of it via the `runExec` override of [[Tool.exec]]. The
  * produced bitstream is loadable with the FOSS programmer ([[OpenFPGALoader]]).
  */
object YosysNextPNR extends Builder:
  val toolName: String = "YosysNextPNR"
  protected def binExec: String = "yosys"
  protected def versionCmd: String = "-V"
  protected def extractVersion(cmdRetStr: String): Option[String] =
    val versionPattern = """Yosys\s+(\d+\.\d+)""".r
    versionPattern.findFirstMatchIn(cmdRetStr).map(_.group(1))

  // platform-aware sibling-executable name (oss-cad-suite ships `.exe` launchers on Windows)
  private def osExe(name: String): String = if (osIsWindows) s"$name.exe" else name

  private def deviceID(using getSet: MemberGetSet): DeviceID =
    getSet.designDB.top.dclMeta.annotations.collectFirst {
      case annotation: DeviceID => annotation
    }.getOrElse(throw new IllegalArgumentException("No device constraint found"))

  // when synthesizing VHDL we drive yosys through the bundled ghdl-yosys-plugin
  private def yosysModuleFlags(using co: CompilerOptions): String = co.backend match
    case _: backends.vhdl => "-m ghdl"
    case _                => ""

  /** An architecture-specific open-source flow. Implementations are nested so they can use the
    * enclosing tool's `exec`/`topName` helpers directly.
    */
  private sealed trait Arch:
    // the yosys synthesis command for this architecture, e.g. "synth_ecp5" / "synth_gowin"
    def synthToolName: String
    // the physical-constraints source file (`.lpf`, `.cst`, ...) consumed by place-and-route
    def constraintsSourceFile(cd: CompiledDesign)(using
        CompilerOptions,
        BuilderOptions
    ): SourceFile
    // place-and-route + bitstream packing, run after yosys synthesis
    def placeAndRoutePack(id: DeviceID)(using
        CompilerOptions,
        BuilderOptions,
        MemberGetSet
    ): Unit
    // files produced by the flow, tracked for caching/cleanup
    def producedFiles(using MemberGetSet): List[String]
  end Arch

  private def archFor(id: DeviceID): Arch = id.vendor match
    case Vendor.Lattice => ECP5
    case Vendor.Gowin   => Gowin
    case vendor         => throw new IllegalArgumentException(
        s"No open-source (yosys+nextpnr) build flow support for vendor $vendor"
      )

  override protected[dfhdl] def buildPreprocess(cd: CompiledDesign)(using
      CompilerOptions,
      BuilderOptions
  ): CompiledDesign =
    given MemberGetSet = cd.stagedDB.getSet
    val arch = archFor(deviceID)
    addSourceFiles(
      cd,
      List(
        new YosysScriptPrinter(arch.synthToolName)(using cd.stagedDB.getSet).getSourceFile,
        arch.constraintsSourceFile(cd)
      )
    )

  def build(
      cd: CompiledDesign
  )(using CompilerOptions, BuilderOptions): CompiledDesign =
    given MemberGetSet = cd.stagedDB.getSet
    // 1. synthesis: yosys reads the committed HDL and emits a netlist JSON
    exec(constructCommand(yosysModuleFlags, s"-s ${topName}.ys"))
    // 2. place-and-route + bitstream packing (architecture specific)
    archFor(deviceID).placeAndRoutePack(deviceID)
    cd
  end build

  override protected[dfhdl] def producedFiles(using
      getSet: MemberGetSet,
      co: CompilerOptions,
      bo: TOptions
  ): List[String] = archFor(deviceID).producedFiles

  // -- Lattice ECP5: synth_ecp5 -> nextpnr-ecp5 -> ecppack (Project Trellis) -------------------

  private object ECP5 extends Arch:
    def synthToolName: String = "synth_ecp5"

    def constraintsSourceFile(cd: CompiledDesign)(using
        CompilerOptions,
        BuilderOptions
    ): SourceFile =
      new NextPNRLatticeConstraintsPrinter(using cd.stagedDB.getSet).getSourceFile

    def producedFiles(using MemberGetSet): List[String] =
      List(s"${topName}.json", s"${topName}.config", s"${topName}.bit")

    // maps the ECP5 LUT-count suffix (e.g. the "85" of LFE5U-85F) to the nextpnr-ecp5 density flag
    private def densityFlag(deviceName: String): String =
      // take the size field after the family prefix (e.g. "LFE5U-85F" -> "85F" -> "85")
      deviceName.split("-").last.filter(_.isDigit) match
        case "12" => "--25k" // the 12F shares the 25k die in Project Trellis
        case "25" => "--25k"
        case "45" => "--45k"
        case "85" => "--85k"
        case _    => throw new IllegalArgumentException(
            s"Unsupported ECP5 device `$deviceName` for the open-source nextpnr-ecp5 flow."
          )

    // maps the Lattice package code (e.g. BG381) to the nextpnr-ecp5 package name (e.g. CABGA381)
    private def packageName(pkg: String): String =
      pkg match
        case "BG256" => "CABGA256"
        case "BG381" => "CABGA381"
        case "BG554" => "CABGA554"
        case "MG285" => "CSFBGA285"
        case "MG381" => "CSFBGA381"
        case _       => throw new IllegalArgumentException(
            s"Unsupported ECP5 package `$pkg` for the open-source nextpnr-ecp5 flow."
          )

    def placeAndRoutePack(id: DeviceID)(using
        co: CompilerOptions,
        bo: BuilderOptions,
        getSet: MemberGetSet
    ): Unit =
      // partName is built as s"$deviceName-$speed$package$grade" (see lattice device defs), so the
      // remainder after the device name is `<speed><package><grade>`.
      val rest = id.partName.stripPrefix(s"${id.deviceName}-")
      val speed = rest.head.toString
      val pkg = rest.drop(1).dropRight(1)
      // place-and-route: nextpnr-ecp5 -> ASCII textcfg
      exec(
        constructCommand(
          densityFlag(id.deviceName),
          s"--package ${packageName(pkg)}",
          s"--speed $speed",
          s"--json ${topName}.json",
          s"--lpf ${topName}.lpf",
          s"--textcfg ${topName}.config"
        ),
        runExec = osExe("nextpnr-ecp5")
      )
      // bitstream packing: ecppack textcfg -> .bit
      exec(
        constructCommand(
          if (bo.compress) "--compress" else "",
          s"${topName}.config",
          s"${topName}.bit"
        ),
        runExec = osExe("ecppack")
      )
    end placeAndRoutePack
  end ECP5

  // -- Gowin GW1N/GW2A: synth_gowin -> nextpnr-himbaechel -> gowin_pack (Project Apicula) -------

  private object Gowin extends Arch:
    def synthToolName: String = "synth_gowin"

    def constraintsSourceFile(cd: CompiledDesign)(using
        CompilerOptions,
        BuilderOptions
    ): SourceFile =
      // the Gowin `.cst` format is shared with the vendor flow and read directly by himbaechel
      new GowinDesignerProjectPhysicalConstraintsPrinter(using cd.stagedDB.getSet).getSourceFile

    def producedFiles(using MemberGetSet): List[String] =
      List(s"${topName}.json", s"${topName}_pnr.json", s"${topName}.fs")

    // gowin_pack `-d` device, derived from the DFHDL device name (e.g. "GW2A-18C")
    private def packDevice(id: DeviceID): String =
      val supported = id.deviceName.startsWith("GW1N") || id.deviceName.startsWith("GW2A")
      if (!supported)
        throw new IllegalArgumentException(
          s"Gowin device `${id.deviceName}` is not supported by the open-source apicula flow " +
            "(only the GW1N and GW2A families are supported)."
        )
      id.deviceName

    def placeAndRoutePack(id: DeviceID)(using
        co: CompilerOptions,
        bo: BuilderOptions,
        getSet: MemberGetSet
    ): Unit =
      val packDev = packDevice(id)
      // place-and-route: nextpnr-himbaechel -> routed JSON. The full part name (e.g.
      // GW2A-LV18PG256C8/I7) selects the apicula device, the device name (e.g. GW2A-18C) selects
      // the himbaechel family, and the `.cst` carries the pin constraints.
      exec(
        constructCommand(
          s"--device ${id.partName}",
          s"--vopt family=${id.deviceName}",
          s"--json ${topName}.json",
          s"--vopt cst=${topName}.cst",
          s"--write ${topName}_pnr.json"
        ),
        runExec = osExe("nextpnr-himbaechel")
      )
      // bitstream packing: gowin_pack routed JSON -> .fs
      exec(
        constructCommand(
          s"-d $packDev",
          s"-o ${topName}.fs",
          s"${topName}_pnr.json"
        ),
        runExec = osExe("gowin_pack")
      )
    end placeAndRoutePack
  end Gowin
end YosysNextPNR

val YosysScript = SourceType.Tool("YosysNextPNR", "Script")

/** Generates the `<top>.ys` yosys script that reads the committed DFHDL output and synthesizes it
  * into a `<top>.json` netlist consumed by nextpnr. Both the Verilog/SystemVerilog and (via the
  * ghdl-yosys-plugin) the VHDL backends are supported.
  */
class YosysScriptPrinter(synthToolName: String)(using
    getSet: MemberGetSet,
    co: CompilerOptions
):
  val designDB: DB = getSet.designDB
  val topName: String = getSet.topName
  val jsonFileName: String = s"$topName.json"
  val scriptFileName: String = s"$topName.ys"

  // the HDL sources to read (headers are pulled in via include folders, so they are excluded here)
  def hdlFiles: List[String] = designDB.srcFiles.collect {
    case SourceFile(
          SourceOrigin.Committed,
          SourceType.Design | SourceType.DFHDLDef | SourceType.GlobalDef,
          path,
          _
        ) if !path.endsWith(".vh") && !path.endsWith(".svh") =>
      path.forceWindowsToLinuxPath
  }

  // folders holding the generated headers/packages, passed to read_verilog as include dirs
  def includeFolders: List[String] = designDB.srcFiles.collect {
    case SourceFile(
          SourceOrigin.Committed,
          SourceType.DFHDLDef | SourceType.GlobalDef,
          path,
          _
        ) =>
      Paths.get(path).getParent.toString.forceWindowsToLinuxPath
  }.distinct

  def contents: String = co.backend match
    case backend: backends.verilog =>
      val svFlag = backend.dialect match
        case VerilogDialect.v95 | VerilogDialect.v2001 => ""
        case _                                         => "-sv "
      val includes = includeFolders.map("-I" + _).mkString(" ")
      sn"""|read_verilog ${svFlag}${includes} ${hdlFiles.mkString(" ")}
           |${synthToolName} -top $topName -json $jsonFileName
           |"""
    case backend: backends.vhdl =>
      val std = backend.dialect match
        case VHDLDialect.v93   => "93"
        case VHDLDialect.v2008 => "08"
        case VHDLDialect.v2019 => "19"
      sn"""|ghdl --std=$std ${hdlFiles.mkString(" ")} -e $topName
           |${synthToolName} -top $topName -json $jsonFileName
           |"""
  end contents

  def getSourceFile: SourceFile =
    SourceFile(SourceOrigin.Compiled, YosysScript, scriptFileName, contents)
end YosysScriptPrinter

val NextPNRLatticeConstraints = SourceType.Tool("YosysNextPNR", "ProjectConstraints")

/** Generates the Project Trellis `.lpf` physical (and clock-frequency) constraints consumed by
  * nextpnr-ecp5. The LOCATE/IOBUF syntax matches the Lattice LPF format; SYSCONFIG is intentionally
  * omitted as nextpnr derives sane defaults and the open-source flow does not need it.
  */
class NextPNRLatticeConstraintsPrinter(using
    getSet: MemberGetSet,
    co: CompilerOptions
):
  val designDB: DB = getSet.designDB
  val topName: String = getSet.topName
  val constraintsFileName: String = s"$topName.lpf"

  def lpf_get_ports(port: DFVal.Dcl, constraint: constraints.SigConstraint): String =
    val portName = port.getName
    port.dfType match
      case DFBit | DFBool => portName
      case _              => constraint.bitIdx match
          case None => throw new IllegalArgumentException(
              s"No bit index constraint found for port ${portName}"
            )
          case bitIdx => s"$portName[$bitIdx]"

  def lpfIOConstraint(
      port: DFVal.Dcl,
      portConstraint: constraints.IO
  ): String =
    var dict = ""
    def addToDict(key: String, value: Any): Unit =
      if (dict.nonEmpty)
        dict += "  "
      dict += s"$key=$value"

    // IO standard constraint
    portConstraint.standard.foreach { standard =>
      val standardStr = standard.withLevelVolt(portConstraint.levelVolt.getOrElse(
        throw new IllegalArgumentException(
          s"No level constraint found for port ${port.getName}"
        )
      ))
      addToDict("IO_TYPE", standardStr)
    }

    // Drive strength constraint
    portConstraint.driveStrength.foreach { driveStrength =>
      addToDict("DRIVE", driveStrength)
    }

    // Pull mode constraint
    portConstraint.pullMode.foreach { pullMode =>
      val pullModeStr = pullMode match
        case constraints.IO.PullMode.UP   => "UP"
        case constraints.IO.PullMode.DOWN => "DOWN"
      addToDict("PULLMODE", pullModeStr)
    }

    val locate =
      s"""LOCATE COMP "${lpf_get_ports(port, portConstraint)}" SITE "${portConstraint.loc}";"""
    val ioBuf =
      if (dict.nonEmpty) s"""IOBUF PORT "${lpf_get_ports(port, portConstraint)}" $dict;"""
      else ""

    sn"""|${locate}
         |${ioBuf}"""
  end lpfIOConstraint

  // nextpnr uses FREQUENCY constraints for timing-driven place-and-route
  def lpfClockConstraint(
      port: DFVal.Dcl,
      constraint: constraints.Timing.Clock
  ): String =
    val mhz = (constraint.rate.get.to_freq / 1.MHz).value.bigDecimal.toPlainString
    s"""FREQUENCY PORT "${port.getName}" $mhz MHZ;"""

  def lpfPortConstraints(port: DFVal.Dcl): List[String] =
    port.meta.annotations.collect {
      case constraint: constraints.IO           => lpfIOConstraint(port, constraint)
      case constraint: constraints.Timing.Clock => lpfClockConstraint(port, constraint)
    }

  def lpfPortConstraints: List[String] =
    designDB.toptopIOs.view.flatMap(lpfPortConstraints).toList

  def contents: String =
    s"""|${lpfPortConstraints.mkString("\n")}
        |""".stripMargin

  def getSourceFile: SourceFile =
    SourceFile(
      SourceOrigin.Compiled,
      NextPNRLatticeConstraints,
      constraintsFileName,
      contents
    )
end NextPNRLatticeConstraintsPrinter
