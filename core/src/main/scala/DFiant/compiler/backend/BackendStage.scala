package DFiant
package compiler
package backend
import DFiant.printer.formatter

import scala.annotation.implicitNotFound

trait BackendStage extends compiler.Compilation.Stage {
  def codeString : String
}
object BackendStage {
  final case class Compilation[D <: DFDesign, B <: BackendStage](
    dsn : D, db : DFDesign.DB, fileSeq : Seq[File]
  ) extends compiler.Compilation[D] {
    def printGenFiles(includeGlobalDefsPackage : Boolean = false) : this.type = {
      val printSeq = if (includeGlobalDefsPackage) fileSeq else fileSeq.drop(1)
      printSeq.foreach {
        case BackendStage.File(fileName, contents) => println(
          s"""\n\n@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
             |@ Contents of $fileName
             |@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
             |$contents
             |""".stripMargin
        )
      }
      this
    }
    def toFolder(folderName : String) : CommittedCompilation[D, B] = {
      import java.io._
      import formatter._
      new java.io.File(folderName).mkdirs()
      //writing entity and architecture files
      val fileNameSeq = fileSeq.map {
        case BackendStage.File(fileName, contents) =>
          val fullName = s"$folderName/$fileName"
          val uncolored = contents.uncolor
          val pw = new FileWriter(new java.io.File(fullName))
          pw.write(uncolored)
          pw.close()
          fullName
      }
      CommittedCompilation[D, B](dsn, db, fileNameSeq)
    }
    def toFile(fileName : String) : CommittedCompilation[D, B] = {
      import java.io._
      import formatter._
      //writing entity and architecture files
      val pw = new FileWriter(new java.io.File(s"$fileName"))
      fileSeq.foreach{
        case BackendStage.File(_, contents) =>
          val uncolored = contents.uncolor
          pw.write(uncolored)
      }
      pw.close()
      CommittedCompilation[D, B](dsn, db, Seq(fileName))
    }
    override def toString : String = s"The Design ${db.top.designType} is compiled. The files are (not committed):\n ${fileSeq.map(f => f.name).mkString(", ")}"
  }

  final case class CommittedCompilation[D <: DFDesign, B <: BackendStage](
    dsn : D, db : DFDesign.DB, fileNameSeq : Seq[String]
  ) extends compiler.Compilation[D] {
    override def toString : String = s"Design ${db.top.designType} committed as the following files:\n ${fileNameSeq.mkString("\n")}"
  }

  @implicitNotFound("Missing a compiler import (e.g., `import compiler.backend.vhdl.v2008`)")
  trait Compiler[B <: BackendStage] {
    def apply[D <: DFDesign, H <: shapeless.HList](c : IRCompilation[D, H]) : BackendStage.Compilation[D, B]
  }

  final case class File(name : String, contents : String)
}
