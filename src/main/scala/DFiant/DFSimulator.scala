package DFiant

import internals._
import DFiant.compiler.Backend

protected[DFiant] class Message(val value : List[Any])(implicit callOwner : DSLOwnerConstruct) extends HasCodeString {
  def codeString: String = "msg\"" + value.collect {
    case x : DFAny => s"$${${x.refCodeString}}"
    case x => x.toString
  }.mkString + "\""
}

protected case class Assert(cond : Option[DFAny], msg : Message, severity : Severity)(implicit ctx0 : DFAny.Op.Context) extends DFAnyMember {
  final val ctx = ctx0
  override private[DFiant] def nameDefault = s"${Name.Separator}assert"
  def codeString : String = cond match {
    case Some(c) =>
      s"""
         |sim.assert(${c.refCodeString}, ${msg.codeString}, ${severity.codeString})""".stripMargin
    case None =>
      s"""
         |sim.report(${msg.codeString}, ${severity.codeString})""".stripMargin
  }
  final val id = getID
  if (cond.isDefined) cond.get.keep
  keep
}

protected sealed trait Severity extends HasCodeString
object Severity {
  case object Note extends Severity {
    def codeString: String = "sim.Note"
  }
  case object Warning extends Severity {
    def codeString: String = "sim.Warning"
  }
  case object Error extends Severity {
    def codeString: String = "sim.Error"
  }
}


trait DFSimulator extends DFDesign {
  private var clkFreqKHz : Int = 100000
  def setClkFreqKHz(clkFreqKHz : Int) : this.type = {this.clkFreqKHz = clkFreqKHz; this}
  private def keepAll : Unit =
    mutableMemberList.collect {
      case m : DFDesign => m.keep.portsOut.foreach(p => p.keep)
      case m => m.keep
    } //for simulations we keep all
  override def compileToVHDL : Backend.VHDL = {
    keepAll
    new Backend.VHDL(this, null, Some(clkFreqKHz))
  }
  override def codeString: String = {
    keepAll
    super.codeString
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
