package DFiant
package sim.tools

import DFiant.compiler.backend.verilog.{Revision, VerilogBackend}
import DFiant.sim.{DFSimDesign, Simulation}

import scala.language.postfixOps
import scala.sys.process._

final case class VerilatorSimulation[D <: DFSimDesign, R <: Revision](
  db : DFDesign.DB,
  fileNameSeq : Seq[String],
  programFile : String = "verilator", //default: assumes the program 'verilator' is in path
  workDir : String = "work",     //default: work folder at current path
  userFlags : String = ""        //default: no additional user flags
)(implicit revision: R) extends Simulation[D, VerilogBackend[R]] {
  def run() : this.type = {
    this
  }
}
