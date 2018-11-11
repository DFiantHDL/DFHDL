package DFiant

import internals._
import DFiant.compiler.Backend

class Message(val value : List[Any])

protected case class Assert(cond : Option[DFAny], msg : Message, severity : Severity)(implicit ctx0 : DFAny.Op.Context) extends DFAnyMember {
  final val ctx = ctx0
  override private[DFiant] def nameDefault = s"${Name.Separator}assert"
  def codeString : String = cond match {
    case Some(c) =>
      s"""
         |sim.assert(${c.refCodeString}, "???", $severity)""".stripMargin
    case None =>
      s"""
         |sim.report("???", $severity)""".stripMargin
  }
  final val id = getID
  keep
}

protected sealed trait Severity
object Severity {
  case object Note extends Severity {
    override def toString: String = "sim.Note"
  }
  case object Warning extends Severity {
    override def toString: String = "sim.Warning"
  }
  case object Error extends Severity {
    override def toString: String = "sim.Error"
  }
}


trait DFSimulator extends DFDesign {
  private var clkFreqKHz : Int = 100000
  def setClkFreqKHz(clkFreqKHz : Int) : this.type = {this.clkFreqKHz = clkFreqKHz; this}
  override def compileToVHDL : Backend.VHDL = {
    mutableMemberList.collect{case m : DFDesign => m.keep.portsOut.foreach(p => p.keep)} //for simulations we keep all
    new Backend.VHDL(this, null, Some(clkFreqKHz))
  }
  protected object sim {
    final val Note = Severity.Note
    final val Warning = Severity.Warning
    final val Error = Severity.Error
    def assert(cond : DFBool, msg : Message, severity : Severity = Warning)(implicit ctx : DFAny.Op.Context) : Unit = {
      Assert(Some(cond), msg, severity)
    }
    def report(msg : Message, severity : Severity = Note)(implicit ctx : DFAny.Op.Context) : Unit = {
      Assert(None, msg, severity)
    }
  }
}
