package DFiant

import internals._
import DFiant.compiler.Backend

protected[DFiant] class Message(value_ : List[Any])(implicit callOwner : DSLOwnerConstruct) extends HasCodeString {
  private def maxLatency : Option[Int] = value_.collect{case x : DFAny => x.thisSourceLB.get.getMaxLatency}.max
  val value : List[Any] = value_.collect {
    case x : DFAny =>
      val elms = x.thisSourceLB.get.balanceTo(maxLatency).elements
      //TODO: fix this
//      assert(elms.length == 1, s"Full handling of split pipeline in a message is not yet supported (${x.fullName})")
      elms.head.tag.get
    case x => x
  }
  def codeString: String = "msg\"" + value_.collect {
    case x : DFAny => s"$${${x.refCodeString}}"
    case x => x.toString
  }.mkString + "\""
  final def keep : Unit = value_.foreach {
    case x : DFAny => x.keep
    case _ =>
  }
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
  msg.keep
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
  case object Failure extends Severity {
    def codeString: String = "sim.Failure"
  }
}

protected case class Finish()(implicit ctx0 : DFAny.Op.Context) extends DFAnyMember {
  final val ctx = ctx0
  override private[DFiant] def nameDefault = s"${Name.Separator}finish"
  def codeString : String =
      s"""
         |sim.finish()""".stripMargin
  final val id = getID
  keep
}


trait DFSimulator extends DFDesign {
  private var clkFreqKHz : Int = 100000
  def setClkFreqKHz(clkFreqKHz : Int) : this.type = {this.clkFreqKHz = clkFreqKHz; this}
  override protected[DFiant] lazy val inSimulation : Boolean = true
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
}
