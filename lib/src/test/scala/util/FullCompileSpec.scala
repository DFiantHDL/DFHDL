package dfhdl.util
import munit.*

import dfhdl.*
import dfhdl.compiler.stages.{CompiledDesign}
import dfhdl.options.{CompilerOptions, LinterOptions}
import tools.linters.*
import java.io.File.separatorChar as S
import java.nio.file.{Files, Paths, Path}
import java.nio.charset.StandardCharsets
import scala.jdk.CollectionConverters.*
import munit.Location

abstract class FullCompileSpec extends FunSuite:
  def dut: core.Design
  given options.CompilerOptions.NewFolderForTop = false
  given options.AppOptions.CacheEnable = false
  def projectFolderName = s"${this.getClass.getPackageName()}.${this.getClass.getSimpleName()}"
  def projectSandboxFolder = s"sandbox${S}FullCompileSpec$S$projectFolderName"
  def projectResourceFolder = s"lib${S}src${S}test${S}resources${S}ref$S$projectFolderName"
  inline given options.CompilerOptions.CommitFolder =
    val backend = compiletime.summonInline[options.CompilerOptions.Backend](backends)
    s"$projectSandboxFolder$S${backend}"
  given options.OnError = _.Exception
  given options.LinterOptions.WError = true
  def verilogLinters(using CompilerOptions): List[LinterOptions._VerilogLinter] =
    List(verilator, iverilog, vlog, xvlog)
  def vhdlLinters(using CompilerOptions): List[LinterOptions._VHDLLinter] =
    List(ghdl, nvc, vcom, xvhdl)
  extension [D <: core.Design](cd: CompiledDesign)
    def lintVerilog(using CompilerOptions): CompiledDesign =
      verilogLinters.foreach { linter =>
        if (linter.isAvailable)
          given LinterOptions.VerilogLinter = _ => linter
          cd.lint
      }
      cd
    def lintVHDL(using CompilerOptions): CompiledDesign =
      vhdlLinters.foreach { linter =>
        if (linter.isAvailable)
          given LinterOptions.VHDLLinter = _ => linter
          cd.lint
      }
      cd
  end extension

  test("verilog[default = sv2009] compilation with no error"):
    given options.CompilerOptions.Backend = _.verilog
    dut.compile.lintVerilog

  test("verilog.v2001 compilation with no error"):
    given options.CompilerOptions.Backend = _.verilog.v2001
    dut.compile.lintVerilog

  test("verilog.v95 compilation with no error"):
    given options.CompilerOptions.Backend = _.verilog.v95
    dut.compile.lintVerilog

  test("vhdl[default = v2008] compilation with no error"):
    given options.CompilerOptions.Backend = _.vhdl
    dut.compile.lintVHDL

  test("vhdl.v93 compilation with no error"):
    given options.CompilerOptions.Backend = _.vhdl.v93
    dut.compile.lintVHDL

  def compareDirectories(
      obtainedDir: String,
      expectedDir: String,
      filter: String => Boolean = _ => true
  )(using Location): Unit =
    val obtainedPath = Paths.get(obtainedDir)
    val expectedPath = Paths.get(expectedDir)

    val obtainedFiles =
      if (Files.exists(obtainedPath))
        Files.walk(obtainedPath).iterator().asScala.filter(Files.isRegularFile(_)).filter(file =>
          filter(obtainedPath.relativize(file).toString)
        ).toList
      else
        List.empty
    val expectedFiles =
      if (Files.exists(expectedPath))
        Files.walk(expectedPath).iterator().asScala.filter(Files.isRegularFile(_)).filter(file =>
          filter(expectedPath.relativize(file).toString)
        ).toList
      else
        List.empty

    val obtainedFileNames = obtainedFiles.map(file => obtainedPath.relativize(file).toString).toSet
    val expectedFileNames = expectedFiles.map(file => expectedPath.relativize(file).toString).toSet

    assertEquals(
      obtainedFileNames,
      expectedFileNames,
      s"Files in $obtainedDir and $expectedDir do not match"
    )

    obtainedFiles.foreach { obtainedFile =>
      val relativePath = obtainedPath.relativize(obtainedFile).toString
      val expectedFile = expectedPath.resolve(relativePath)
      val obtainedContents =
        Files.readAllLines(obtainedFile, StandardCharsets.UTF_8).asScala.mkString("\n")
      val expectedContents =
        Files.readAllLines(expectedFile, StandardCharsets.UTF_8).asScala.mkString("\n")
      assertNoDiff(
        obtainedContents,
        expectedContents,
        s"Contents of $relativePath do not match"
      )
    }
  end compareDirectories

  test("same generated files in verilog and vhdl folders"):
    compareDirectories(
      projectSandboxFolder,
      projectResourceFolder,
      fileName =>
        """.*\.(v|sv|vh|svh|vhd)$""".r.matches(fileName) &&
          !""".*(dfhdl_pkg|dfhdl_defs)\.(vh|svh|vhd)$""".r.matches(fileName)
    )
end FullCompileSpec
