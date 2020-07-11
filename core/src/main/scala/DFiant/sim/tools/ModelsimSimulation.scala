package DFiant
package sim.tools

import java.io.{File, FileWriter}

import DFiant.compiler.backend.Backend
import DFiant.compiler.backend.verilog.VerilogBackend
import DFiant.compiler.backend.vhdl.VHDLBackend
import compiler.backend.verilog
import compiler.backend.vhdl
import DFiant.sim.{DFSimDesign, Simulation}

import scala.language.postfixOps
import scala.sys.process._

final case class ModelsimSimulation[D <: DFSimDesign, B <: Backend.Stage](
  db : DFDesign.DB,
  fileNameSeq : Seq[String],
  modelsimDir : String = "", //default: assumes the program 'vcom' is in path
  workDir : String = "work" //default: work folder at current path
)(implicit supportedBackend: ModelsimSimulation.SupportedBackend[B]) extends Simulation[D, B] {
  private val topFilePath = fileNameSeq.last
  private val simFilesDir = {
    val f = new File(topFilePath)
    f.getParentFile.getAbsolutePath
  }
  private val workPath = s"$simFilesDir\\$workDir"
  private val topName = db.top.designType
  private val vcomPath = if (modelsimDir.isEmpty) "vcom" else s"$modelsimDir\\vcom"
  private val vlogPath = if (modelsimDir.isEmpty) "vlog" else s"$modelsimDir\\vlog"
  private val vlibPath = if (modelsimDir.isEmpty) "vlib" else s"$modelsimDir\\vlib"
  private val vsimPath = if (modelsimDir.isEmpty) "vsim" else s"$modelsimDir\\vsim"
  private val mkWorkCmd : String = s"$vlibPath $workPath"
  private def vhdlCompileCmd(revisionCmd : String) : String = s"$vcomPath -work $workPath $revisionCmd ${fileNameSeq.mkString(" ")}"
  private val verilogCompileCmd : String = s"$vlogPath -work $workPath ${fileNameSeq.mkString(" ")}"
  private val runsimCmd : String = s"""$vsimPath -lib $workPath $topName -t 1ps -do "set NumericStdNoWarnings 1" -do "run -all""""

  private def createWorkDir() : Unit = {
    val f = new File(workPath)
    if (!f.exists()) {
      mkWorkCmd !!
    }
  }
  private def compileSim() : Unit = {
    println("Compiling Modelsim simulation... ")
    supportedBackend.backend match {
      case vlog : verilog.VerilogBackend[_] =>
        verilogCompileCmd !!
      case vcom : vhdl.VHDLBackend[_] =>
        val revisionCmd = vcom.revision match {
          case vhdl.Revision.V93 => "-93"
          case vhdl.Revision.V2008 => "-2008"
        }
        vhdlCompileCmd(revisionCmd) !!
    }
  }
  private lazy val initSim : Unit = {
    createWorkDir()
    compileSim()
  }
  def run() : this.type = {
    initSim
    println("Running simulation...")

    {runsimCmd !}
    this
  }
}

object ModelsimSimulation {
  sealed trait SupportedBackend[B <: Backend.Stage] {
    val backend : B
  }
  implicit def verilogSupport[R <: verilog.Revision](
    implicit revision0 : R
  ) : SupportedBackend[verilog.VerilogBackend[R]] = new SupportedBackend[verilog.VerilogBackend[R]] {
    val backend : VerilogBackend[R] = new VerilogBackend[R] {
      val revision : R = revision0
    }
  }
  implicit def vhdlSupport[R <: vhdl.Revision](
    implicit revision0 : R
  ) : SupportedBackend[vhdl.VHDLBackend[R]] = new SupportedBackend[vhdl.VHDLBackend[R]] {
    val backend : VHDLBackend[R] = new VHDLBackend[R] {
      val revision : R = revision0
    }
  }
}