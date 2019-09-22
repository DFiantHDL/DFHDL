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

import scala.collection.immutable

abstract class DFComponent[Comp <: DFComponent[Comp]](implicit ctx : DFComponent.Context[Comp], args : sourcecode.Args)
  extends DFDesign with DSLFoldableOwnerConstruct { self : Comp =>

  protected[DFiant] trait __DevDFComponent extends __DevDFDesign with __DevDSLFoldableOwnerConstruct {
    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Naming
    /////////////////////////////////////////////////////////////////////////////////////////////////////////
//    override lazy val typeName: String = self.getClass.getSimpleName
    def foldedConstructCodeString : String = {
      ctx.compName.value + args.value.dropRight(1).map(e => e.map(f => f.value).mkString("(",", ",")")).mkString
    }
    private[DFiant] override def constructCodeString : String = if (isFolded) foldedConstructCodeString else super.constructCodeString
    override def codeString : String = {
      valCodeString
    }

    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Member discovery
    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    private lazy val temp : CacheBoxRO[Set[DFAnyMember]] =
      CacheDerivedRO(portsIn, super.discoveryDependencies)(super.discoveryDependencies ++ portsIn)
    @inline override private[DFiant] def discoveryDependencies : CacheBoxRO[Set[DFAnyMember]] = temp
//    override lazy val discoveredSet : CacheBoxRO[Set[DFAnyMember]] =
//      CacheDerivedRO(keepMembers, discoveredOutputs, foldRequest) {
//        discover(Set(), discoveredOutputs)
//      }

    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Folding/Unfolding
    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    final override private[DFiant] def preFoldUnfold(): Unit = {
      super.preFoldUnfold()
      portsOut.foreach(p => p.preFoldUnfold()) //clear output ports assignments and connections when folding
    }
    final override private[DFiant] def unfoldedRun : Unit = {
      ctx.impl(self)
//      portsOut.foreach(p => p.rediscoverDependencies())
    }

    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Initialization
    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    final def initOf[DF <: DFAny](dfVal : DF) : CacheBoxRO[Seq[dfVal.TToken]] = {
      val ff = foldedFunctions(dfVal)
      val inputInits = ff.inputs.map(i => i.initCB)

      CacheDerivedRO(inputInits) {
        ff.init.asInstanceOf[Seq[dfVal.TToken]]
      }
    }
  }
  override private[DFiant] lazy val __dev : __DevDFComponent = new __DevDFComponent {}

  abstract class FoldedFunction[O <: DFAny] private (val output : O)(val inputs : List[DFAny]) {
    def init : Seq[output.TToken]

  }
  object FoldedFunction {
    def apply[O <: DFAny, L <: DFAny, R <: DFAny](o : O)(l : L, r : R)(func : (l.TToken, r.TToken) => o.TToken) =
      new FoldedFunction(o)(List(l, r)) {
        def init: Seq[output.TToken] = DFAny.TokenSeq(l.initCB.unbox, r.initCB.unbox)(func).asInstanceOf[Seq[output.TToken]]
      }
  }
  protected val foldedFunctions : Map[DFAny, FoldedFunction[_]] = Map()
  protected val foldedDiscoveryDependencyList : List[Tuple2[DFAny.Port[_ <: DFAny, _ <: OUT],List[DFAny.Port[_ <: DFAny, _ <: IN]]]]

  final protected def setInitFunc[DFVal <: DFAny.Initializable[_]](dfVal : DFVal)(value : LazyBox[Seq[dfVal.TToken]])
  : Unit = dfVal.setInitFunc.forced(value)
  final protected def getInit[DFVal <: DFAny.Initializable[_]](dfVal : DFVal) : LazyBox[Seq[dfVal.TToken]] = dfVal.initLB

  final class InPortExtended(dfVal : DFAny.Port[_ <: DFAny, _ <: IN]) {
    def isOpen : Boolean = !dfVal.isConnected
    def isConstant : Boolean = dfVal.__dev.isConstant
  }
  final implicit def InPortExtended(dfVal: DFAny.Port[_ <: DFAny, _ <: IN]): InPortExtended = new InPortExtended(dfVal)
}

object DFComponent {
  implicit def fetchDev(from : DFComponent[_])(implicit devAccess: DFiant.dev.Access) : from.__dev.type = from.__dev

  trait Context[Comp <: DFComponent[Comp]] extends DFBlock.ContextOf[Unit, DFBlock] {
    implicit val impl : Comp => Unit
    val compName : sourcecode.Name.OfType[Comp]
  }
  trait LowPriority {
    implicit def evFromOpContext[Comp <: DFComponent[Comp]](
      implicit
      evContext : DFAny.Op.Context,
      evImpl : Comp => Unit,
      evMeta : Meta,
      evCompName : sourcecode.Name.OfType[Comp],
      forceNotVar : Meta.ForceNotVar[Context[_]]
    ) : Context[Comp] = new Context[Comp] {
      val ownerOption : Option[DFBlock] = evContext.ownerOption
      implicit val impl: Comp => Unit = evImpl
      implicit val targetLib: TargetLib = evContext.targetLib
      implicit val config: DFAnyConfiguration = evContext.config
      val meta: Meta = evMeta
      val compName = evCompName
    }
  }
  object Context extends LowPriority {
    implicit def ev[Comp <: DFComponent[Comp]](
      implicit
      evOwner : DFBlock,
      evImpl : Comp => Unit,
      evBasicLib : TargetLib,
      evConfig : DFAnyConfiguration,
      evMeta : Meta,
      evCompName : sourcecode.Name.OfType[Comp],
      forceNotVar : Meta.ForceNotVar[Context[_]]
    ) : Context[Comp] = new Context[Comp] {
      val ownerOption : Option[DFBlock] = Option(evOwner)
      implicit val impl: Comp => Unit = evImpl
      implicit val targetLib: TargetLib = evBasicLib
      implicit val config: DFAnyConfiguration = evConfig
      val meta: Meta = evMeta
      val compName = evCompName
    }
  }
}



