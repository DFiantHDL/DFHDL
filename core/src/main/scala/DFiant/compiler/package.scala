package DFiant

import DFiant.DFAny.Modifier
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
    implicit class SourceOps(source : Source) {
      def relSlice : List[SourceElement] =
        source.bitsWL(sourceElement.relWidth, sourceElement.relBitLow).elements
    }
    def getDependencies(dependencyKind : DependencyKind)(
      implicit dependencyContext: DependencyContext
    ) : List[SourceElement] = dependencyKind match {
      //The dependency is for init, but the source element is not referenced with an init,
      //so there is no dependency to return
      case DependencyKind.Init if !sourceElement.withInit => Nil
      case _ =>
        import dependencyContext.getSet
        sourceElement.srcVal match {
          case SourceValue.Const(_) => Nil
          //Toplevel inputs have no dependencies
          case SourceValue.Dcl(dcl, _) if dcl.isTopLevelInput => Nil
          case SourceValue.Dcl(dcl, version) => dependencyKind match {
              //An initialized declaration doesn't have dependencies that can affect further
              case DependencyKind.Init if (dcl.externalInit.isDefined) => Nil
              case DependencyKind.Init => version match {
                //There are no init dependencies if there is no connection
                case SourceVersion.Empty => Nil
                //Use latest connection/assignment
                case SourceVersion.Latest => dcl.getSource.relSlice
                //For init dependency, only connections are used.
                //Assignments are equivalent to empty connections.
                case SourceVersion.Idx(block, idx) => dependencyContext.assignmentMap(dcl)(block)(idx)._2.relSlice
                case SourceVersion.IfElse(_, branchVersions, fallbackVersion) => Nil
                case SourceVersion.Match(_, caseVersions, fallbackVersionOption) => Nil
              }
              case DependencyKind.Data => version match {
                //Data dependency when no connection/assignment is implicitly using the previous data
                case SourceVersion.Empty => dcl.getSource.prev(1).relSlice
                //Use latest connection/assignment
                case SourceVersion.Latest => dcl.getSource.relSlice
                //Use specific version of connection/assignment (except special cases)
                case SourceVersion.Idx(block, idx) => dependencyContext.assignmentMap(dcl)(block)(idx)._2.relSlice
                case SourceVersion.IfElse(_, branchVersions, fallbackVersion) =>
                  sourceElement.versioned(dcl, fallbackVersion) :: branchVersions.flatMap { case (condSrc, version) =>
                    List(condSrc.elements.head, sourceElement.versioned(dcl, version))
                  }
                case SourceVersion.Match(_, caseVersions, fallbackVersionOption) =>
                  val caseElements = caseVersions.map { case (_, version) =>
                    sourceElement.versioned(dcl, version)
                  }
                  fallbackVersionOption
                    .map(sourceElement.versioned(dcl, _))
                    .map(_ :: caseElements)
                    .getOrElse(caseElements)
              }
            }
          case SourceValue.Func2(func) =>
            List(func.leftArgRef.getSource, func.rightArgRef.getSource).flatMap(_.elements)
          case SourceValue.ApplySel(_, relSrc, idxSrc) =>
            relSrc.elements ++ idxSrc.elements
        }
      }
  }
  final implicit class DclSourceOps(dcl : DFAny.Dcl) {
    def getSource(implicit dependencyContext: DependencyContext) : Source = {
      import dependencyContext.getSet
      dependencyContext.getLatestSource(dcl)(dcl.getOwnerDesign)
    }
  }
  final implicit class RefSourceOps(ref : DependencyContext.ConsumeRef) {
    def getSource(implicit dependencyContext: DependencyContext) : Source =
      dependencyContext.dependencyMap(ref)
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
  type EvaluationMap[T] = Map[SourceValue, T]
  implicit class EvaluationMapOps[T](evaluationMap: EvaluationMap[T])(implicit elementEvaluator: ElementEvaluator[T]) {
    def evaluate(sourceElement: SourceElement) : T =
      elementEvaluator.applyElement(evaluationMap(sourceElement.srcVal), sourceElement)
  }
}
