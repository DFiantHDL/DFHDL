/*
 *     This file is part of DFiant.
 *
 *     DFiant is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU Lesser General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     any later version.
 *
 *     DFiant is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     GNU Lesser General Public License for more details.
 *
 *     You should have received a copy of the GNU Lesser General Public License
 *     along with DFiant.  If not, see <https://www.gnu.org/licenses/>.
 */

package DFiant

import internals._
import DFiant.compiler.Backend

import scala.collection.immutable

trait DFAnySimMember extends DFAnyMember

//TODO: change DFString to a synthesizable dataflow variable
protected[DFiant] class DFString(value_ : List[Any])(implicit ctx0 : DFAny.Op.Context) extends DFAnySimMember {
  final private[DFiant] override lazy val ctx = ctx0
  protected[DFiant] trait __DevMessage extends __DevDFAnyMember {
    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Member discovery
    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    @inline override private[DFiant] def discoveryDependenciesStatic : Set[DFAnyMember] =
      super.discoveryDependenciesStatic ++ value_.collect{case v : DFAny => v}

//    private def maxLatency: Option[Int] = value_.collect { case x: DFAny => x.thisSourceLB.get.getMaxLatency }.max

    val versioned : List[Either[Source, Any]] = value_.map {
      case x : DFAny => Left(x.source.versioned)
      case x => Right(x)
    }

    val value: List[Any] = value_.collect {
      case x: DFAny =>
        val elms = x.source.elements//.get.balanceTo(maxLatency).elements
        //TODO: fix this
        //      assert(elms.length == 1, s"Full handling of split pipeline in a message is not yet supported (${x.fullName})")
        elms.head
      case x => x
    }

    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Naming
    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    override lazy val nameScala = s"${Meta.Name.AnonStart}message"
    def codeString: String = "msg\"" + value_.collect {
      case x: DFAny => s"$${${x.refCodeString}}"
      case x => x.toString
    }.mkString + "\""
  }
  override private[DFiant] lazy val __dev : __DevMessage = new __DevMessage {}
  import __dev._
  id
}

protected case class Assert(cond : Option[DFAny], msg : DFString, severity : Severity)(implicit ctx0 : DFAny.Op.Context) extends DFAnySimMember {
  final private[DFiant] override lazy val ctx = ctx0
  protected[DFiant] trait __DevAssert extends __DevDFAnyMember {
    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Member discovery
    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    @inline override private[DFiant] def discoveryDependenciesStatic : Set[DFAnyMember] =
      super.discoveryDependenciesStatic ++ cond.toList

    final val condVersionedSource = cond.map(c => c.source.versioned)

    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Naming
    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    override lazy val nameScala = s"${Meta.Name.Separator}assert"
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
  id
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
  final private[DFiant] override lazy val ctx = ctx0
  protected[DFiant] trait __DevFinish extends __DevDFAnyMember {
    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Naming
    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    override lazy val nameScala = s"${Meta.Name.Separator}finish"
    def codeString : String =
      s"""
         |sim.finish()""".stripMargin
  }
  override private[DFiant] lazy val __dev : __DevFinish = new __DevFinish {}
  import __dev._
  id
}


trait DFSimulator extends DFDesign {
  protected[DFiant] trait __DevDFSimulator extends __DevDFDesign {
    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Member discovery
    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    //for simulation we discover all top design output ports
    private lazy val _discoveryDependencies : CacheBoxRO[Set[DFAnyMember]] = CacheDerivedRO(members, super.discoveryDependencies)(
      super.discoveryDependencies ++ members.flatMap{case m : DFDesign => m.portsOut}
    )
    @inline override private[DFiant] def discoveryDependencies : CacheBoxRO[Set[DFAnyMember]] = _discoveryDependencies

    //for simulation we discover all direct members and the top modules output ports
    override lazy val discoveredSet : CacheBoxRO[Set[DFAnyMember]] = CacheDerivedRO(members) {
      discover(Set(), members)
    }
  }
  override private[DFiant] lazy val __dev : __DevDFSimulator = new __DevDFSimulator {}
  import __dev._

  private var clkFreqKHz : Int = 100000
  def setClkFreqKHz(clkFreqKHz : Int) : this.type = {this.clkFreqKHz = clkFreqKHz; this}
  override protected[DFiant] lazy val inSimulation : Boolean = true
  override def compileToVHDL : Backend.VHDL = new Backend.VHDL(this, null, Some(clkFreqKHz))
}
