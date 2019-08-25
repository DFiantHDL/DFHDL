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
//    override lazy val discoveredSet : CacheBoxRO[immutable.HashSet[DFAnyMember]] =
//      CacheDerivedRO(keepMembers, discoveredOutputs, foldRequest) {
//        discover(immutable.HashSet(), discoveredOutputs)
//      }
//    override def postDiscoveryRun() : Unit = foldedDiscoveryDependencyList.collect {case Tuple2(out, inList) =>
//      out.injectDependencies(inList)
//      out.rediscoverDependencies()
//    }

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
  }
  override private[DFiant] lazy val __dev : __DevDFComponent = new __DevDFComponent {}

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
  implicit def fetchDev[Comp <: DFComponent[Comp]](from : DFComponent[Comp])(implicit devAccess: DFiant.dev.Access) : from.__dev.type = from.__dev

  trait Context[Comp <: DFComponent[Comp]] extends DFBlock.ContextOf[Unit, DFBlock] {
    implicit val impl : Comp => Unit
    val compName : sourcecode.Name.OfType[Comp]
  }
  trait LowPriority {
    implicit def evFromOpContext[Comp <: DFComponent[Comp]](
      implicit
      evContext : DFAny.Op.Context,
      evImpl : Comp => Unit,
      evNameIt : NameIt,
      evCompName : sourcecode.Name.OfType[Comp],
      forceNotVar : NameIt.ForceNotVar[Context[_]]
    ) : Context[Comp] = new Context[Comp] {
      val ownerOption : Option[DFBlock] = evContext.ownerOption
      implicit val impl: Comp => Unit = evImpl
      implicit val targetLib: TargetLib = evContext.targetLib
      implicit val config: DFAnyConfiguration = evContext.config
      val n: NameIt = evNameIt
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
      evNameIt : NameIt,
      evCompName : sourcecode.Name.OfType[Comp],
      forceNotVar : NameIt.ForceNotVar[Context[_]]
    ) : Context[Comp] = new Context[Comp] {
      val ownerOption : Option[DFBlock] = Option(evOwner)
      implicit val impl: Comp => Unit = evImpl
      implicit val targetLib: TargetLib = evBasicLib
      implicit val config: DFAnyConfiguration = evConfig
      val n: NameIt = evNameIt
      val compName = evCompName
    }
  }
}



