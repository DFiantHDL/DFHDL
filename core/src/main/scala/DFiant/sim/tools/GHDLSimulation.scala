package DFiant
package sim.tools

import DFiant.compiler.backend.vhdl.{Revision, Backend}
import DFiant.sim.{DFSimDesign, Simulation}

import sys.process._
import scala.language.postfixOps

final case class GHDLSimulation[D <: DFSimDesign, R <: Revision](
  db : DFDesign.DB,
  fileNameSeq : Seq[String],
  programFile : String = "ghdl", //default: assumes the program 'ghdl' is in path
  workDir : String = "work",     //default: work folder at current path
  userFlags : String = ""        //default: no additional user flags
)(implicit revision: R) extends Simulation[D, Backend[R]] {
  private val workDirFlag = s"--workdir=$workDir"
  private val std = (revision : Revision) match {
    case Revision.V2008 => "08"
    case Revision.V93 => "93"
  }
  private val flags = s"$workDirFlag -frelaxed-rules --ieee=synopsys --std=$std $userFlags"
  private val files = fileNameSeq.mkString(" ")
  private val topEntity = db.top.designType

  private def prepareWork() : Unit = {
    new java.io.File(workDir).mkdirs()
    s"$programFile --clean $workDirFlag" !!
  }
  private def addFiles() : Unit = {
    s"$programFile -a $flags $files" !!
  }
  private lazy val initSim : Unit = {
    prepareWork()
    addFiles()
  }


  def run() : this.type = {
    initSim

    {s"$programFile -r $flags $topEntity --ieee-asserts=disable" !}
    this
  }

}
