package DFiant
package compiler
package backend
import printer.Printer

import scala.annotation.implicitNotFound

object Backend {
  trait Stage extends compiler.Compilation.Stage

  final case class Compilation[D <: DFDesign, B <: Stage](
    db : DFDesign.DB, fileSeq : Seq[File]
  ) extends compiler.Compilation[D] {
    def printGenFiles()(implicit printConfig : Printer.Config) : this.type = {
      import printConfig.formatter._
      fileSeq.foreach {
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
    def toFolder(folderName : String)(implicit printConfig : Printer.Config) : CommittedCompilation[D, B] = {
      import java.io._
      import printConfig.formatter._
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
    def toFile(fileName : String)(implicit printConfig : Printer.Config) : CommittedCompilation[D, B] = {
      import java.io._
      import printConfig.formatter._
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
  }

  final case class CommittedCompilation[D <: DFDesign, B <: Stage](
    db : DFDesign.DB, fileNameSeq : Seq[String]
  ) extends compiler.Compilation[D]

  @implicitNotFound("Missing a compiler import (e.g., `import compiler.backend.vhdl.v2008`)")
  trait Compiler[B <: Stage] {
    def apply[D <: DFDesign, H <: shapeless.HList](c : IRCompilation[D, H]) : Backend.Compilation[D, B]
  }

  final case class File(name : String, contents : String)
}
