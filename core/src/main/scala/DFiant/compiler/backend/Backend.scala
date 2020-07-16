package DFiant
package compiler
package backend
import DFiant.printer.formatter

import scala.annotation.implicitNotFound

object Backend {
  trait Stage extends compiler.Compilation.Stage {
    def codeString : String
  }

  final case class Compilation[D <: DFDesign, B <: Stage](
    db : DFDesign.DB, fileSeq : Seq[File]
  ) extends compiler.Compilation[D] {
    def printGenFiles(includeGlobalDefsPackage : Boolean = false) : this.type = {
      val printSeq = if (includeGlobalDefsPackage) fileSeq else fileSeq.drop(1)
      printSeq.foreach {
        case Backend.File(fileName, contents) => println(
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
        case Backend.File(fileName, contents) =>
          val fullName = s"$folderName/$fileName"
          val uncolored = contents.uncolor
          val pw = new FileWriter(new java.io.File(fullName))
          pw.write(uncolored)
          pw.close()
          fullName
      }
      CommittedCompilation[D, B](db, fileNameSeq)
    }
    def toFile(fileName : String) : CommittedCompilation[D, B] = {
      import java.io._
      import formatter._
      //writing entity and architecture files
      val pw = new FileWriter(new java.io.File(s"$fileName"))
      fileSeq.foreach{
        case Backend.File(_, contents) =>
          val uncolored = contents.uncolor
          pw.write(uncolored)
      }
      pw.close()
      CommittedCompilation[D, B](db, Seq(fileName))
    }
    override def toString : String = s"The Design ${db.top.designType} is compiled. The files are (not committed):\n ${fileSeq.map(f => f.name).mkString(", ")}"
  }

  final case class CommittedCompilation[D <: DFDesign, B <: Stage](
    db : DFDesign.DB, fileNameSeq : Seq[String]
  ) extends compiler.Compilation[D] {
    override def toString : String = s"Design ${db.top.designType} committed as the following files:\n ${fileNameSeq.mkString("\n")}"
  }

  @implicitNotFound("Missing a compiler import (e.g., `import compiler.backend.vhdl.v2008`)")
  trait Compiler[B <: Stage] {
    def apply[D <: DFDesign, H <: shapeless.HList](c : IRCompilation[D, H]) : Backend.Compilation[D, B]
  }

  final case class File(name : String, contents : String)
}
