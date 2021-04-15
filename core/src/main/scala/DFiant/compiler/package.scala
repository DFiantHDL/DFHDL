package DFiant

import DFiant.DFAny.{Alias, Modifier}
import DFiant.DFNet.Op
import DFiant.compiler.analysis.IfBlockAnalysis
import compiler.backend._

import scala.annotation.tailrec
import scala.collection.immutable.BitSet
package object compiler {
  implicit def SanityCheck[D <: DFDesign, C](c : C)(implicit conv : C => IRCompilation[D])
  : SanityCheck[D] = new SanityCheck[D](c)
  implicit def FixAnonymous[D <: DFDesign, C](c : C)(implicit conv : C => IRCompilation[D])
  : FixAnonymous[D] = new FixAnonymous[D](c)
  implicit def evOrderMembers[D <: DFDesign, C](c : C)(implicit conv : C => IRCompilation[D])
  : OrderMembers[D] = new OrderMembers[D](c)
  implicit def ForceOthersCaseCoverage[D <: DFDesign, C](c : C)(implicit conv : C => IRCompilation[D])
  : ForceOthersCaseCoverage[D] = new ForceOthersCaseCoverage[D](c)
  implicit def UniqueDesigns[D <: DFDesign, C](c : C)(implicit conv : C => IRCompilation[D])
  : UniqueDesigns[D] = new UniqueDesigns[D](c)
  implicit def NamedSelection[D <: DFDesign, C](c : C)(implicit conv : C => IRCompilation[D])
  : NamedAliases[D] = new NamedAliases[D](c)
  implicit def UniqueNames[D <: DFDesign, C](c : C)(implicit conv : C => IRCompilation[D])
  : UniqueNames[D] = new UniqueNames[D](c)
  implicit def FlattenNames[D <: DFDesign, C](c : C)(implicit conv : C => IRCompilation[D])
  : FlattenNames[D] = new FlattenNames[D](c)
  implicit def FlattenTypes[D <: DFDesign, C](c : C)(implicit conv : C => IRCompilation[D])
  : FlattenTypes[D] = new FlattenTypes[D](c)
  implicit def ConvertMatchToIf[D <: DFDesign, C](c : C)(implicit conv : C => IRCompilation[D])
  : ConvertMatchToIf[D] = new ConvertMatchToIf[D](c)
  implicit def ExplicitPrev[D <: DFDesign, C](c : C)(implicit conv : C => IRCompilation[D])
  : ExplicitPrev[D] = new ExplicitPrev[D](c)
  implicit def ExplicitConversions[D <: DFDesign, C](c : C)(implicit conv : C => IRCompilation[D])
  : ExplicitConversions[D] = new ExplicitConversions[D](c)
  implicit def ExplicitNamedVars[D <: DFDesign, C](c : C)(implicit conv : C => IRCompilation[D])
  : ExplicitNamedVars[D] = new ExplicitNamedVars[D](c)
  implicit def ViaPortConnection[D <: DFDesign, C](c : C)(implicit conv : C => IRCompilation[D])
  : ViaPortConnection[D] = new ViaPortConnection[D](c)
  implicit def MoveCBDesigns[D <: DFDesign, C](c : C)(implicit conv : C => IRCompilation[D])
  : MoveCBDesigns[D] = new MoveCBDesigns[D](c)
  implicit def ControlDesigns[D <: DFDesign, C](c : C)(implicit conv : C => IRCompilation[D])
  : ControlDesigns[D] = new ControlDesigns[D](c)
  implicit def Flatten[D <: DFDesign, C](c : C)(implicit conv : C => IRCompilation[D])
  : Flatten[D] = new Flatten[D](c)
  implicit def Calculator[D <: DFDesign, C](c : C)(implicit conv : C => IRCompilation[D])
  : InitCalc[D] = new InitCalc[D](c)
  implicit def PathEvaluator[D <: DFDesign, C](c : C)(implicit conv : C => IRCompilation[D])
  : PathEvaluator[D] = new PathEvaluator[D](c)
  implicit def Pipelining[D <: DFDesign, C](c : C)(implicit conv : C => IRCompilation[D])
  : Pipelining[D] = new Pipelining[D](c)
  implicit def SingleStepPrev[D <: DFDesign, C](c : C)(implicit conv : C => IRCompilation[D])
  : SingleStepPrev[D] = new SingleStepPrev[D](c)
  implicit def RTL[D <: DFDesign, C](c : C)(implicit conv : C => IRCompilation[D])
  : RTL[D] = new RTL[D](c)

  trait PreCompiler[D <: DFDesign] {
    def apply(fromStage : IRCompilation[D]) : IRCompilation[D]
  }
  object PreCompiler {
    implicit def defaultDoesNothing[D <: DFDesign] : PreCompiler[D] =
      (fromStage : IRCompilation[D]) => fromStage
  }
  trait PostCompiler[D <: DFDesign, B <: BackendStage] {
    def apply(fromStage : BackendStage.Compilation[D, B]) : BackendStage.Compilation[D, B]
  }
  object PostCompiler {
    implicit def defaultDoesNothing[D <: DFDesign, B <: BackendStage] : PostCompiler[D, B] =
      (fromStage : BackendStage.Compilation[D, B]) => fromStage
  }

  sealed trait DependencyKind extends Product with Serializable
  object DependencyKind {
    case object Data extends DependencyKind
    case object Init extends DependencyKind
  }
  final implicit class SourceElementOps(sourceElement: SourceElement) {
    //    implicit class SourceOps(source : Source) {
    //      def relSlice : List[SourceElement] =
    //        source.bitsWL(sourceElement.relWidth, sourceElement.relBitLow).elements
    //    }

    def getDependencies(dependencyKind : DependencyKind)(
      implicit dependencyContext : DependencyContext
    ) : List[SourceElement] = {
      import dependencyContext.getSet
      (dependencyKind, sourceElement.srcVal.member, sourceElement.srcVal.version) match {
        //The dependency is for init, but the source element is not referenced with an init,
        //so there is no dependency to return
        case (DependencyKind.Init, _, _) if !sourceElement.withInit => Nil
        //Constants has no dependencies
        case (_, _ : DFAny.Const, _) => Nil
        //Top-level inputs have no dependencies
        case (_, dcl : DFAny.Dcl, _) if dcl.isTopLevelInput => Nil
        //An initialized declaration doesn't have dependencies that can affect further
        case (DependencyKind.Init, dcl : DFAny.Dcl, _) if dcl.externalInit.isDefined => Nil
        //There are no init dependencies if there is no connection
        case (DependencyKind.Init, _ : DFAny.Dcl, SourceVersion.Empty) => Nil
        //An empty data dependency leads to a dependency on its latest value via Prev
        case (DependencyKind.Data, dcl : DFAny.Dcl, SourceVersion.Empty) => Nil //Source.Latest(dcl).elements
        //Use specific version of connection/assignment
        case (_, dcl : DFAny.Dcl, SourceVersion.Idx(block, idx)) =>
          //TODO: maybe for init we need to filter-out elements that don't have withInit ?
          dependencyContext.assignmentMap(dcl)(block)(idx)._2.elements
        case (_, dcl : DFAny.Dcl, SourceVersion.Latest) =>
          dcl.getSource.elements
        case (_, dcl : DFAny.Dcl, SourceVersion.IfElse(_, branchVersions, fallbackVersion)) =>
          sourceElement.versioned(dcl, fallbackVersion) :: branchVersions.flatMap { case (condSrc, version) =>
            List(condSrc.elements.head, sourceElement.versioned(dcl, version))
          }
        case (_, dcl : DFAny.Dcl, SourceVersion.Match(_, caseVersions, fallbackVersionOption)) =>
            val caseElements = caseVersions.map { case (_, version) =>
              sourceElement.versioned(dcl, version)
            }
            fallbackVersionOption
              .map(sourceElement.versioned(dcl, _))
              .map(_ :: caseElements)
              .getOrElse(caseElements)
        case (_, DFAny.ApplySel(_, _, relValRef, idxRef, _, _), SourceVersion.Latest) =>
          relValRef.getSource.elements ++ idxRef.getSource.elements
        //Alias handling
        case (_, alias : DFAny.Alias, SourceVersion.Latest) =>
          alias.relValRef.getSource.elements
          //TODO: in the future special case partial bit dependency
          //            case Alias.BitsWL(_, _, _, relWidth, relBitLow, _, _) =>
          //              relSrc.bitsWL(relWidth, relBitLow).elements
        //Single argument function handling
        case (_, func : DFAny.Func1, SourceVersion.Latest) => func.leftArgRef.getSource.elements
        //Infix function handling
        case (_, func : DFAny.Func2, SourceVersion.Latest) =>
          func.leftArgRef.getSource.elements ++ func.rightArgRef.getSource.elements
        case x =>
          println(x)
          ???
      }
    }
  }


  final implicit class DclSourceOps(member : DFAny.Member) {
    def getSource(implicit dependencyContext: DependencyContext) : Source = {
      import dependencyContext.getSet
      dependencyContext.getLatestSource(member)(member.getOwnerDesign)
    }
  }
  final implicit class RefSourceOps1(ref : DependencyContext.ConsumeRef) {
    def getSource(implicit dependencyContext: DependencyContext) : Source =
      dependencyContext.dependencyMap(ref)
  }
  final implicit class RefSourceOps2(ref : DFAny.Alias.RelValRef) {
    def getSource(implicit dependencyContext: DependencyContext) : Source = {
      import dependencyContext.getSet
      ref.get.getSource
    }
  }
  final implicit class RefSourceOps3(ref : DFAny.ApplySel.RelValRef) {
    def getSource(implicit dependencyContext: DependencyContext) : Source = {
      import dependencyContext.getSet
      ref.get.getSource
    }
  }
  final implicit class BlockVersionsOps(blockVersions: DependencyContext.BlockVersions) {
    @tailrec def getLatestVersionIn(
      currentBlock : DFBlock
    )(implicit getSet: MemberGetSet) : SourceVersion = {
      blockVersions.get(currentBlock) match {
        case Some(versions) => SourceVersion.Idx(currentBlock, versions.length-1)
        case None => currentBlock match {
          case _ : DFDesign.Block => SourceVersion.Empty
          case cb : DFConditional.Block => getLatestVersionIn(cb.getOwnerBlock)
        }
      }
    }
  }

  type PendingTable = Map[SourceValue, BitSet]
  object PendingTable {
    def empty : PendingTable = Map.empty[SourceValue, BitSet]
  }
  implicit class PendingTableOps(pendingTable: PendingTable) {
    def waitsFor(sourceElement: SourceElement) : Boolean =
      pendingTable.get(sourceElement.srcVal) match {
        case Some(bitSet) =>
          val elementBitSet = BitSet.fromSpecific(sourceElement.relBitLow to sourceElement.relBitHigh)
          if ((bitSet & elementBitSet).nonEmpty) true
          else false
        case None => false
      }
    def addPending(sourceElement: SourceElement) : PendingTable = {
      val elementBitSet = BitSet.fromSpecific(sourceElement.relBitLow to sourceElement.relBitHigh)
      pendingTable.get(sourceElement.srcVal) match {
        case Some(bitSet) => pendingTable.updated(sourceElement.srcVal, bitSet | elementBitSet)
        case None => pendingTable + (sourceElement.srcVal -> elementBitSet)
      }
    }
  }
  type SourceValueEvaluation[T] = Map[SourceValue, T]
  implicit class EvaluationMapOps[T](srcValEvaluation: SourceValueEvaluation[T])(implicit elementEvaluator: ElementEvaluator[T]) {
    def evaluate(srcVal : SourceValue) : T = srcValEvaluation(srcVal)
    def evaluate(sourceElement: SourceElement) : T =
      elementEvaluator.applyElement(srcValEvaluation(sourceElement.srcVal), sourceElement)
  }
}
