/*
 *     This file is part of DFiant.
 *
 *     DFiant is free software: you can redistribute it and/or modify
 *     it under the terms of the Lesser GNU General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     any later version.
 *
 *     DFiant is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     Lesser GNU General Public License for more details.
 *
 *     You should have received a copy of the Lesser GNU General Public License
 *     along with DFiant.  If not, see <https://www.gnu.org/licenses/>.
 */

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
      elms.head.aliasTag.get
    case x => x
  }
  def codeString: String = "msg\"" + value_.collect {
    case x : DFAny => s"$${${x.refCodeString}}"
    case x => x.toString
  }.mkString + "\""
  final def keep() : Unit = value_.foreach {
    case x : DFAny => x.keep
    case _ =>
  }
  final def consume() : Unit = value_.foreach {
    case x : DFAny => x.consume()
    case _ =>
  }
}

trait DFAnySimMember extends DFAnyMember

protected case class Assert(cond : Option[DFAny], msg : Message, severity : Severity)(implicit ctx0 : DFAny.Op.Context) extends DFAnySimMember {
  final private[DFiant] lazy val ctx = ctx0
  protected[DFiant] trait __DevAssert extends __DevDFAnyMember {
    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Naming
    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    override protected def nameDefault = s"${Name.Separator}assert"
    def codeString : String = cond match {
      case Some(c) =>
        s"""
           |sim.assert(${c.refCodeString}, ${msg.codeString}, ${severity.codeString})""".stripMargin
      case None =>
        s"""
           |sim.report(${msg.codeString}, ${severity.codeString})""".stripMargin
    }
  }
  override private[DFiant] lazy val __dev : __DevAssert = new __DevAssert {}
  import __dev._

  if (cond.isDefined) {
    cond.get.keep
    cond.get.consume()
  }
  msg.keep()
  msg.consume()
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

protected case class Finish()(implicit ctx0 : DFAny.Op.Context) extends DFAnySimMember {
  final private[DFiant] lazy val ctx = ctx0
  protected[DFiant] trait __DevFinish extends __DevDFAnyMember {
    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Naming
    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    override protected def nameDefault = s"${Name.Separator}finish"
    def codeString : String =
      s"""
         |sim.finish()""".stripMargin
  }
  override private[DFiant] lazy val __dev : __DevFinish = new __DevFinish {}
  import __dev._
  keep
}


trait DFSimulator extends DFDesign {
  protected[DFiant] trait __DevDFSimulator extends __DevDFDesign {
    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Naming
    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    override def codeString: String = {
      keepAll()
      super.codeString
    }

    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Member discovery
    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    private[DFSimulator] def keepAll() : Unit =
      __dev.members.collect {
        case m : DFDesign => m.keep.portsOut.foreach(p => p.keep)
        case m => m.keep
      } //for simulations we keep all
  }
  override private[DFiant] lazy val __dev : __DevDFSimulator = new __DevDFSimulator {}
  import __dev._

  private var clkFreqKHz : Int = 100000
  def setClkFreqKHz(clkFreqKHz : Int) : this.type = {this.clkFreqKHz = clkFreqKHz; this}
  override protected[DFiant] lazy val inSimulation : Boolean = true
  override def compileToVHDL : Backend.VHDL = {
    keepAll()
    new Backend.VHDL(this, null, Some(clkFreqKHz))
  }
}
