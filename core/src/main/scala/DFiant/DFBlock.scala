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

import DFiant.targetlib.TargetLib
import internals._

import scala.annotation.{implicitNotFound, tailrec}

abstract class DFBlock(implicit ctx0 : DFBlock.Context) extends DFAnyOwner with Implicits {self =>
  final private[DFiant] override lazy val ctx = ctx0
  protected[DFiant] trait __DevDFBlock extends __DevDFAnyOwner {
    ////////////////////////////////////////////////////////////////////////// //////////////////////////////
    // Ownership
    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    final val topDsn : DFDesign =
      ownerOption.map(o => o.asInstanceOf[DFBlock].topDsn).getOrElse(self.asInstanceOf[DFDesign])

    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Connections and Assignment Nets
    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    final val netsTo : CacheBoxRO[Map[DFAny, List[Either[Source, DFBlock]]]] =
      CacheDerivedHashMapRO(addedMembers)(Map[DFAny, List[Either[Source, DFBlock]]]()) {
        case (hm, c : ConditionalBlock[_,_]) => //For child conditional DFBlocks we just add a placeholder
          val childCons = c.netsTo.map {
            case (dfVal, _) => dfVal -> (hm.getOrElse(dfVal, List()) :+ Right(c))
          }
          hm ++ childCons
        case (hm, c : DFNet) =>
          var bitH : Int = c.toVal.width-1
          val fromValSourceVersioned = c.fromVal.source.via(c).versioned
          val cons = c.toVal.source.elements.collect {
            case SourceElement(relBitHigh, relBitLow, reverseBits, Some(t)) =>
              val relWidth = relBitHigh - relBitLow + 1
              val bitL = bitH-relWidth+1
              val partial = fromValSourceVersioned.bitsHL(bitH, bitL).reverse(reverseBits)
              val current = hm.getOrElse(t.dfVal, List())
              val empty = Source.none(t.dfVal.width)
              val list = current :+ Left(empty.replaceHL(relBitHigh, relBitLow, partial))
              bitH = bitH-relWidth
              t.dfVal -> list
          }
          hm ++ cons
        case (hm, _) => hm
      }

    final def netsToAt(dfVal : DFAny, relBitWidth : Int, relBitLow : Int) : List[Either[Source, DFBlock]] = {
      assert(!dfVal.isInstanceOf[DFAny.Alias[_]], "unexpected call to function with an alias")
      val complete = netsTo.getOrElse(dfVal, List())
      if (dfVal.width.getValue == relBitWidth) complete
      else complete.flatMap {
        case Left(src) =>
          val partial = src.bitsWL(relBitWidth, relBitLow)
          if (partial.isEmpty) None
          else Some(Left(partial))
        case Right(block) =>
          val partial = block.netsToAt(dfVal, relBitWidth, relBitLow)
          if (partial.isEmpty) None
          else Some(Right(block))
      }
    }

    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Simulation
    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    private[DFiant] def hasSimMembers : Boolean = members.collectFirst{
      case m : DFAnySimMember => m
      case m : DFBlock if m.hasSimMembers => m
    }.nonEmpty
  }
  override private[DFiant] lazy val __dev : __DevDFBlock = ???
  import __dev._

  override protected[DFiant] type ThisOwner = DFBlock
  private[DFiant] implicit val mutableOwner : MutableOwner = new MutableOwner(this)
  final protected implicit val protInternalContext : DFBlock.InternalContext = DFBlock.InternalContext
  override implicit def __theOwnerToBe : DFBlock = mutableOwner.value
  implicit val targetLib = ctx.targetLib

  final protected[DFiant] object ifdf extends ConditionalBlock.IfNoRetVal(mutableOwner)
  final protected[DFiant] object matchdf extends ConditionalBlock.MatchNoRetVal(mutableOwner)
  protected def selectdf[T <: DFAny](cond : DFBool)(thenSel : T, elseSel : T) : T = ???
  protected def selectdf[SW, T <: DFAny](sel : DFUInt[SW], default : => Option[T] = None)(args : List[T]) : T = ???

  protected object sim {
    final val Note = Severity.Note
    final val Warning = Severity.Warning
    final val Error = Severity.Error
    final val Failure = Severity.Failure
    def assert(cond : DFBool, msg : DFString, severity : Severity = Warning) : Unit = {
      if (inSimulation) Assert(Some(cond.replacement()), msg, severity)(ctx.updateOwner(__theOwnerToBe))
    }
    def report(msg : DFString, severity : Severity = Note) : Unit = {
      if (inSimulation) Assert(None, msg, severity)(ctx.updateOwner(__theOwnerToBe))
    }
    def finish() : Unit = {
      if (inSimulation) Finish()(ctx.updateOwner(__theOwnerToBe))
    }
  }
}
object DFBlock {
  implicit def fetchDev(from : DFBlock)(implicit devAccess: DFiant.dev.Access) : from.__dev.type = from.__dev
  @implicitNotFound(errors.MissingContext.msg)
  trait ContextOf[+T, +Owner <: DFAnyOwner] extends DFAnyOwner.ContextWithLibOf[T, Owner] {
    self =>
    def updateOwner[Owner0 <: DFAnyOwner](owner0 : Owner0)(implicit n0 : Meta) : ContextOf[T, Owner0] = new ContextOf[T, Owner0] {
      val ownerOption : Option[Owner0] = Some(owner0)
      implicit val targetLib: TargetLib = self.targetLib
      implicit val config: DFAnyConfiguration = self.config
      val meta: Meta = n0
    }
  }
  trait LowestPriority {
    implicit def evTop[T, Owner <: DFAnyOwner](
      implicit
      lp : shapeless.LowPriority,
      evAllowTop : DFDesign.AllowTOP, //Must have an implicit AllowTOP in scope
      evBasicLib : TargetLib,
      evConfig : DFAnyConfiguration,
      evMeta : Meta,
      forceNotVar : Meta.ForceNotVar[ContextOf[_,_]]
    ) : ContextOf[T, Owner] = new ContextOf[T, Owner] {
      val ownerOption : Option[Owner] = None
      implicit val targetLib: TargetLib = evBasicLib
      implicit val config: DFAnyConfiguration = evConfig
      val meta: Meta = evMeta
    }
  }
  trait LowPriority extends LowestPriority {
    implicit def ev[T, Owner <: DFAnyOwner](
      implicit
      lp : shapeless.LowPriority,
      evOwner : Owner,
      evBasicLib : TargetLib,
      evConfig : DFAnyConfiguration,
      evMeta : Meta,
      forceNotVar : Meta.ForceNotVar[ContextOf[_,_]]
    ) : ContextOf[T, Owner] = new ContextOf[T, Owner] {
      val ownerOption : Option[Owner] = Option(evOwner)
      implicit val targetLib: TargetLib = evBasicLib
      implicit val config: DFAnyConfiguration = evConfig
      val meta: Meta = evMeta
    }
  }
  private[DFiant] sealed trait InternalContext
  object InternalContext extends InternalContext
  object ContextOf extends LowPriority {
    implicit def evContext[T, T2](
      implicit
      lp : shapeless.LowPriority,
      evContext : DFDesign.ContextOf[T2],
      external : shapeless.Refute[InternalContext],
      forceNotVar : Meta.ForceNotVar[ContextOf[_,_]]
    ) : ContextOf[T, DFBlock] = new ContextOf[T, DFBlock] {
      val ownerOption : Option[DFBlock] = evContext.ownerOption
      implicit val targetLib : TargetLib = evContext.targetLib
      implicit val config : DFAnyConfiguration = evContext.config
      val meta : Meta = evContext.meta
    }
  }
  type Context = ContextOf[Unit, DFBlock]
}



class MutableOwner(var value : DFBlock)

