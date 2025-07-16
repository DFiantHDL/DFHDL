//format: off
import sbt._
import Keys._
import java.nio.file.{Files, Paths, StandardCopyOption, Path}
import scala.jdk.CollectionConverters._
import java.nio.charset.StandardCharsets

object DFHDLCommands {
  val quickTestSetup = Command.command("quickTestSetup") { state =>
    val extracted = Project.extract(state)
    val newState = extracted.appendWithSession(Seq(
      (LocalProject("internals") / Test / sources) := Nil,
      (LocalProject("core") / Test / sources) := Nil,
      (LocalProject("compiler_stages") / Test / sources) := Nil,
      (LocalProject("devices") / Compile / sources) := Nil,
      (LocalProject("devices") / Test / sources) := Nil,
      (LocalProject("lib") / Test / sources) := ((LocalProject("lib") / Test / sources).value.filter(_.toString.contains("Playground.scala")))
    ), state)
    newState
  }

  val docExamplesRefUpdate = Command.command("docExamplesRefUpdate") { state =>
    val sandboxDir = Paths.get("sandbox/FullCompileSpec")
    val targetDir = Paths.get("lib/src/test/resources/ref")
    val filter: String => Boolean =
      fileName =>
        fileName.matches(""".*\.(v|sv|vh|svh|vhd)$""") &&
        !fileName.matches(""".*(dfhdl_pkg|dfhdl_defs)\.(vh|svh|vhd)$""")

    def copyFiles(source: Path, target: Path, filter: String => Boolean): Unit = {
      if (Files.isDirectory(source)) {
        if (!Files.exists(target)) {
          Files.createDirectories(target)
        }
        val sourceFiles = Files.walk(source).iterator().asScala.filter(Files.isRegularFile(_)).filter(file => filter(source.relativize(file).toString)).toList
        val targetFiles = Files.walk(target).iterator().asScala.filter(Files.isRegularFile(_)).toList

        // Copy or overwrite files from source to target
        sourceFiles.foreach { file =>
          val relativePath = source.relativize(file)
          val targetFile = target.resolve(relativePath)
          if (!Files.exists(targetFile.getParent)) {
            Files.createDirectories(targetFile.getParent)
          }
          if (Files.exists(targetFile)) {
            val sourceContent = new String(Files.readAllBytes(file), StandardCharsets.UTF_8).replace("\r\n", "\n")
            val targetContent = new String(Files.readAllBytes(targetFile), StandardCharsets.UTF_8).replace("\r\n", "\n")
            if (sourceContent != targetContent) {
              Files.copy(file.toAbsolutePath(), targetFile.toAbsolutePath(), StandardCopyOption.REPLACE_EXISTING)
            }
          } else {
            Files.copy(file.toAbsolutePath(), targetFile.toAbsolutePath(), StandardCopyOption.REPLACE_EXISTING)
          }
        }

        // Remove files in target that are not in source
        val sourceFileNames = sourceFiles.map(file => source.relativize(file).toString).toSet
        targetFiles.foreach { file =>
          val relativePath = target.relativize(file).toString
          if (!sourceFileNames.contains(relativePath)) {
            Files.delete(file)
          }
        }
      }
    }

    copyFiles(sandboxDir, targetDir, filter)

    state
  }

  val clearSandbox = Command.command("clearSandbox") { state =>
    // Remove the sandbox folder and all its contents before running the tests
    val sandboxPath = java.nio.file.Paths.get("sandbox")
    if (java.nio.file.Files.exists(sandboxPath)) {
      import java.nio.file._
      import java.nio.file.attribute.BasicFileAttributes

      Files.walkFileTree(
        sandboxPath,
        new SimpleFileVisitor[Path] {
          override def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult = {
            Files.delete(file)
            FileVisitResult.CONTINUE
          }
          override def postVisitDirectory(dir: Path, exc: java.io.IOException): FileVisitResult = {
            Files.delete(dir)
            FileVisitResult.CONTINUE
          }
        }
      )
    }
    state
  }

  val vhdlTools = List("ghdl", "nvc", "questa", "vivado")
  val verilogTools = List("verilator", "iverilog", "questa", "vivado")
  val vhdlDialects = List("vhdl.v93", "vhdl.v2008")
  val verilogDialects = List("verilog.v95", "verilog.v2001", "verilog.sv2005")

  val testApps = Command.command("testApps") { state =>
    var newState = Command.process("clearSandbox", state, _ => ())
    val extracted = Project.extract(newState)
    val runMainTask = LocalProject("lib") / Test / runMain
    val existingTools: Set[String] = {
      import java.io.{ByteArrayOutputStream, PrintStream}
      val baos = new ByteArrayOutputStream()
      val ps = new PrintStream(baos)
      val oldOut = System.out
      val oldErr = System.err
      val arguments = s" util.top_EmptyDesign help simulate-tool -s"
      try {
        System.setOut(ps)
        System.setErr(ps)
        val extracted = Project.extract(state)
        extracted.runInputTask(runMainTask, arguments, state)
      } finally {
        System.out.flush()
        System.err.flush()
        System.setOut(oldOut)
        System.setErr(oldErr)
      }
      val helpStr = baos.toString("UTF-8")
      val allTools = (vhdlTools ++ verilogTools).toSet
      allTools.filter(tool => helpStr.linesIterator.exists(line => line.contains(tool) && line.contains("Found version")))
    }
    for (tool <- vhdlTools if existingTools.contains(tool); dialect <- vhdlDialects) {
      val arguments = s" AES.top_CipherSim simulate -b $dialect -t $tool --Werror-tool"
      val (updatedState, _) = extracted.runInputTask(runMainTask, arguments, newState)
      newState = updatedState
    }
    for (tool <- verilogTools if existingTools.contains(tool); dialect <- verilogDialects) {
      val arguments = s" AES.top_CipherSim simulate -b $dialect -t $tool --Werror-tool"
      val (updatedState, _) = extracted.runInputTask(runMainTask, arguments, newState)
      newState = updatedState
    }
    newState
  }
}