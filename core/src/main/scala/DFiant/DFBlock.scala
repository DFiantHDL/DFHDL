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
import scala.collection.immutable

abstract class DFBlock(implicit ctx0 : DFBlock.Context) extends DFAnyOwner with Implicits {self =>
  final private[DFiant] override lazy val ctx = ctx0
  protected[DFiant] trait __DevDFBlock extends __DevDFAnyOwner {
    ////////////////////////////////////////////////////////////////////////// //////////////////////////////
    // Ownership
    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    final val topDsn : DFDesign =
      ownerOption.map(o => o.asInstanceOf[DFBlock].topDsn).getOrElse(self.asInstanceOf[DFDesign])

    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Assignments
    /////////////////////////////////////////////////////////////////////////////////////////////////////////
//    final val assignmentsTo = CacheDerivedHashMapRO(members)(immutable.HashMap[DFAny, List[DFAny.Assignment]]()) {
//      case (hm, a : DFAny.Assignment) => hm.get(a.toVar) match {
//        case Some(la) => hm + (a.toVar -> (la :+ a))
//        case None => hm + (a.toVar -> List(a))
//      }
//      case (hm, _) => hm
//    }
//
    final val assignmentsTo : CacheBoxRO[immutable.HashMap[DFAny, List[Source]]] =
      CacheDerivedHashMapRO(members)(immutable.HashMap[DFAny, List[Source]]()) {
        case (hm, c : ConditionalBlock with DFBlock) =>
//          println(c.members)
//          val childCons = c.assignmentsTo.map {
//            case (dfVal, sources) => dfVal -> (hm.getOrElse(dfVal, List()) ++ sources)
//          }
          hm// ++ childCons
        case (hm, c : DFAny.Assignment) =>
          def versioned(source : Source) : Source = Source(source.elements.map {
            case SourceElement(relBitHigh, relBitLow, reverseBits, Some(t)) => t match {
              case AliasTag(dfVal, context, version, prevStep, inverted, latency, pipeStep) if dfVal.isAssignable =>
                val defaultContext = dfVal.owner.asInstanceOf[DFBlock]
                @tailrec def getAssignedSource(currentContext : DFBlock) : AliasTag = currentContext.assignmentsTo.get.get(dfVal) match {
                  case Some(x) => AliasTag(dfVal, currentContext, Some(x.length), prevStep, inverted, latency, pipeStep)
                  case None => currentContext match {
                    case x : DFBlock with ConditionalBlock => getAssignedSource(x.owner)
                    case _ => AliasTag(dfVal, currentContext, Some(0), prevStep, inverted, latency, pipeStep)
                  }
                }
                SourceElement(relBitHigh, relBitLow, reverseBits, Some(getAssignedSource(defaultContext)))
              case _ => SourceElement(relBitHigh, relBitLow, reverseBits, Some(t))
            }
            case e => e
          })

          var bitH : Int = c.toVar.width-1
          val fromValSourceVersioned = versioned(c.fromVal.source)
          val cons = c.toVar.source.elements.collect {
            case SourceElement(relBitHigh, relBitLow, reverseBits, Some(t)) =>
              val relWidth = relBitHigh - relBitLow + 1
              val bitL = bitH-relWidth+1
              val partial = fromValSourceVersioned.bitsHL(bitH, bitL).reverse(reverseBits)
              val current = hm.getOrElse(t.dfVal, List())
              val full = current match {
                case x :+ xs => current :+ xs.replaceHL(relBitHigh, relBitLow, partial)
                case Nil => List(Source.none(t.dfVal.width).replaceHL(relBitHigh, relBitLow, partial))
              }
              bitH = bitH-relWidth
              t.dfVal -> full
          }
          hm ++ cons
        case (hm, _) => hm
      }

    final val assignmentsFrom = CacheDerivedHashMapRO(members)(immutable.HashMap[DFAny, List[DFAny.Assignment]]()) {
      case (hm, a : DFAny.Assignment) => hm.get(a.fromVal) match {
        case Some(la) => hm + (a.fromVal -> (la :+ a))
        case None => hm + (a.fromVal -> List(a))
      }
      case (hm, _) => hm
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
    def assert(cond : DFBool, msg : Message, severity : Severity = Warning) : Unit = {
      if (inSimulation) Assert(Some(cond.replacement()), msg, severity)(ctx.updateOwner(__theOwnerToBe))
    }
    def report(msg : Message, severity : Severity = Note) : Unit = {
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
    def updateOwner[Owner0 <: DFAnyOwner](owner0 : Owner0)(implicit n0 : NameIt) : ContextOf[T, Owner0] = new ContextOf[T, Owner0] {
      val ownerOption : Option[Owner0] = Some(owner0)
      implicit val targetLib: TargetLib = self.targetLib
      implicit val config: DFAnyConfiguration = self.config
      val n: NameIt = n0
    }
  }
  trait LowestPriority {
    implicit def evTop[T, Owner <: DFAnyOwner](
      implicit
      lp : shapeless.LowPriority,
      evAllowTop : DFDesign.AllowTOP, //Must have an implicit AllowTOP in scope
      evBasicLib : TargetLib,
      evConfig : DFAnyConfiguration,
      evNameIt : NameIt,
      forceNotVar : NameIt.ForceNotVar[ContextOf[_,_]]
    ) : ContextOf[T, Owner] = new ContextOf[T, Owner] {
      val ownerOption : Option[Owner] = None
      implicit val targetLib: TargetLib = evBasicLib
      implicit val config: DFAnyConfiguration = evConfig
      val n: NameIt = evNameIt
    }
  }
  trait LowPriority extends LowestPriority {
    implicit def ev[T, Owner <: DFAnyOwner](
      implicit
      lp : shapeless.LowPriority,
      evOwner : Owner,
      evBasicLib : TargetLib,
      evConfig : DFAnyConfiguration,
      evNameIt : NameIt,
      forceNotVar : NameIt.ForceNotVar[ContextOf[_,_]]
    ) : ContextOf[T, Owner] = new ContextOf[T, Owner] {
      val ownerOption : Option[Owner] = Option(evOwner)
      implicit val targetLib: TargetLib = evBasicLib
      implicit val config: DFAnyConfiguration = evConfig
      val n: NameIt = evNameIt
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
      forceNotVar : NameIt.ForceNotVar[ContextOf[_,_]]
    ) : ContextOf[T, DFBlock] = new ContextOf[T, DFBlock] {
      val ownerOption : Option[DFBlock] = evContext.ownerOption
      implicit val targetLib : TargetLib = evContext.targetLib
      implicit val config : DFAnyConfiguration = evContext.config
      val n : NameIt = evContext.n
    }
  }
  type Context = ContextOf[Unit, DFBlock]
}



class MutableOwner(var value : DFBlock)

