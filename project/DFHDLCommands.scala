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
      (LocalProject("lib") / Test / sources) := ((LocalProject("lib") / Test / sources).value.filter(_.toString.contains("Example.scala")))
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
}