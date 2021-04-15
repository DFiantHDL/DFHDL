package DFiant
package compiler

import DFiant.DFAny.Alias.Prev
import DFiant.DFAny.Func2.Op
import DFiant.compiler.csprinter.CSPrinter
import DFiant.internals.IntExtras
import PipelineEvaluator.{Pipe, PipeDelayEstimator, PipelineInfo}


class PipelineEvaluator(
  pipelineMethod: PipelineMethod, delayEstimator: PipeDelayEstimator
) extends ElementEvaluator[PipelineInfo] {
  def applyElement(t : => PipelineInfo, element : SourceElement) : PipelineInfo =
    if (element.relWidth != element.srcVal.member.width) t.bitsWL(element.relWidth, element.relBitLow)
    else t

  protected def concat(dfType : DFAny.Type, elements : List[PipelineInfo]) : PipelineInfo =
    elements match {
      case head :: Nil => head
      case _ => PipelineInfo.Join(elements)
    }

  def apply(srcVal : SourceValue)(
    implicit srcValEvaluation : SourceValueEvaluation[PipelineInfo], dependencyContext : DependencyContext
  ) : PipelineInfo = {
    import dependencyContext.getSet
    implicit val constEvaluator = ConstEvaluator
    dependencyContext.constMap.evaluate(srcVal) match {
      //a constant (non bubble token value) will always yield no pipeline and no delay
      case token if !token.isBubble => PipelineInfo.Node(token.width, 0, Pipe.NA)
      case _ => srcVal match {
        //new input yields a new pipeline path
        case SourceValue(dcl : DFAny.Dcl, _) if dcl.isTopLevelInput =>
          PipelineInfo.Node(dcl.width, 0, Pipe.Accumulation(0))
        case SourceValue(dcl : DFAny.Dcl, version) => version match {
          //no assignment means a commit state
          case SourceVersion.Empty => PipelineInfo.Node(dcl.width, 0, Pipe.Cyclic)
          //All other declaration accesses rely on the dependency of the relevant assignment
          case SourceVersion.Idx(block, idx) =>
            dependencyContext.assignmentMap(dcl)(block)(idx)._2.evaluate(dcl.dfType)
          case SourceVersion.Latest =>
            dcl.getSource.evaluate(dcl.dfType)
          case SourceVersion.IfElse(_, branchVersions, fallbackVersion) =>
            val branchInfos = branchVersions.map(bv => (bv._1.evaluate(DFBool.Type(true)), dcl.evaluate(bv._2)))
            val fallBackInfo = dcl.evaluate(fallbackVersion)
            PipelineInfo.IfElse(dcl.width, branchInfos, fallBackInfo)
          case SourceVersion.Match(headCase, caseVersions, fallbackVersionOption) => ???
        }
        case SourceValue(member, SourceVersion.Latest) => member match {
          //Currently an unary function has very little effect over delays, so we choose to ignore it
          case func : DFAny.Func1 =>
            func.leftArgRef.evaluate
          case prev : DFAny.Alias.Prev =>
            val relInfo = prev.relValRef.evaluate
            (prev.kind, pipelineMethod) match {
              //State prev only affects the delay, but not the pipeline
              case (Prev.State, _) => relInfo.zeroDelay
              //No pipelining is forced
              case (Prev.Pipe, PipelineMethod.NoPipeline) => relInfo
              //Manual pipelining
              case (Prev.Pipe, _) => prev.relValRef.get match {
                //Manually pipelined func2 overrides the stages set by the automatic pipelining
                case func : DFAny.Func2 if func.tags.meta.namePosition == prev.tags.meta.namePosition | func.isAnonymous =>
                  relInfo match {
                    case funcInfo : PipelineInfo.Func2 => funcInfo.copy(extraStages = prev.step)
                    case _ => ??? //unexpected
                  }
                case _ => relInfo.pipe(prev.step)
              }
            }
          case func : DFAny.Func2 =>
            val leftJoin = func.leftArgRef.evaluate
            val rightJoin = func.rightArgRef.evaluate
            val estimations = delayEstimator.func2(func)
            val extraStages = pipelineMethod match {
              case PipelineMethod.NoPipeline => 0 //forced no pipeline
              case PipelineMethod.ManualPipeline => 0 //pipeline is set by the user only
              case PipelineMethod.AutoPipeline(targetMaxDelay) => //automatic pipeline
                val joinInfo = PipelineInfo.Join(List(leftJoin, rightJoin))
                estimations.indexWhere { e =>
                  e.maxDelay < targetMaxDelay && (e.consumeDelay + joinInfo.accDelay) < targetMaxDelay
                } match {
                  case extraPipe if extraPipe >= 0 => extraPipe
                  case _ => estimations.length
                }
            }
            PipelineInfo.Func2(func.width, leftJoin, rightJoin, extraStages, estimations)
          case alias : DFAny.Alias.AsIs =>
            alias.relValRef.evaluate.node.bitsWL(alias.width, 0)
          case alias : DFAny.Alias.BitsWL =>
            alias.relValRef.evaluate.node.bitsWL(alias.relWidth, alias.relBitLow)
          case applySel : DFAny.ApplySel =>
            val relValConst = applySel.relValRef.evaluate
            val idxConst = applySel.idxRef.evaluate
            ???
        }
        case x =>
          println(x)
          ???
      }
    }
  }

  def cyclic(srcVal : SourceValue)(
    implicit srcValEvaluation : SourceValueEvaluation[PipelineInfo], dependencyContext : DependencyContext
  ) : PipelineInfo = PipelineInfo.Node(srcVal.member.width, 0, Pipe.Cyclic)

  def codeString(t : PipelineInfo)(implicit printer : CSPrinter) : String = t.toString
}

sealed trait PipelineMethod extends Product with Serializable
object PipelineMethod {
  //Without any pipeline stages, including manual pipe stages constraint by the designer
  case object NoPipeline extends PipelineMethod
  //Only manual pipe stages as constrained by the designer
  case object ManualPipeline extends PipelineMethod
  //Automatic pipeline, including the manual pipeline constraints set by the user
  final case class AutoPipeline(targetMaxDelay : Int) extends PipelineMethod
}

object PipelineEvaluator {
  sealed trait Pipe extends Product with Serializable {
    import Pipe._
    def max (that : Pipe) : Pipe = (this, that) match {
      case (Accumulation(value), NA | Cyclic) => Accumulation(value)
      case (NA | Cyclic, Accumulation(value)) => Accumulation(value)
      case (Cyclic, NA | Cyclic) => Cyclic
      case (NA , Cyclic) => Cyclic
      case (NA, NA) => NA
      case (Accumulation(l), Accumulation(r)) => Accumulation(l max r)
    }
    def pipe (extraStages : Int) : Pipe = this match {
      case NA => Accumulation(extraStages)
      case Cyclic => Cyclic
      case Accumulation(stages) => Accumulation(stages + extraStages)
    }
  }
  object Pipe {
    case object Cyclic extends Pipe
    case object NA extends Pipe
    final case class Accumulation(stages : Int) extends Pipe
  }
  sealed trait PipelineInfo extends Product with Serializable {
    val width : Int
    /**
      * Accumulated delay
      */
    val accDelay : Int
    /**
      * Accumulated pipe
      */
    val accPipe : Pipe
    final def zeroDelay : PipelineInfo.Node = PipelineInfo.Node(width, 0, accPipe)
    final def pipe(extraStages : Int) : PipelineInfo.ExtraPipe =
      PipelineInfo.ExtraPipe(this, extraStages)
    final def node : PipelineInfo.Node = PipelineInfo.Node(width, accDelay, accPipe)
    def bitsWL(relWidth : Int, relBitLow : Int) : PipelineInfo
  }
  object PipelineInfo {
    import Pipe._
    final case class Node(width : Int, accDelay : Int, accPipe : Pipe) extends PipelineInfo {left =>
      def delayBy(extraDelay : Int) : Node = copy(accDelay = accDelay + extraDelay)
      def bitsWL(relWidth : Int, relBitLow : Int) : PipelineInfo = copy(width = relWidth)
    }
    final case class ExtraPipe(relInfo : PipelineInfo, extraStages : Int) extends PipelineInfo {
      val width : Int = relInfo.width
      val accDelay : Int = if (extraStages > 0) 0 else relInfo.accDelay
      val accPipe : Pipe = relInfo.accPipe.pipe(extraStages)
      def pipeRequired : Boolean = extraStages > 0
      def bitsWL(relWidth : Int, relBitLow : Int) : PipelineInfo =
        ExtraPipe(relInfo.bitsWL(relWidth, relBitLow), extraStages)
    }
    final case class Join(paths : List[PipelineInfo]) extends PipelineInfo {
      val width : Int = paths.map(_.width).sum
      val accPipe : Pipe = paths.map(_.accPipe).reduce(_ max _)
      val extraInputPipe : List[ExtraPipe] = {
        accPipe match {
          case Pipe.Cyclic | Pipe.NA => paths.map(p => ExtraPipe(p, 0))
          case Accumulation(max) =>
            paths.map(p => p.accPipe match {
              case Pipe.Cyclic | Pipe.NA => ExtraPipe(p, 0)
              case Accumulation(stages) => ExtraPipe(p, max - stages)
            })
        }
      }
      def pipeRequired : Boolean = extraInputPipe.exists(ep => ep.extraStages > 0)
      val accDelay : Int = paths.map(_.accDelay).lazyZip(extraInputPipe).map {
        case (delay, extraPipe) => if (extraPipe.extraStages > 0) 0 else delay
      }.max
      def bitsWL(relWidth : Int, relBitLow : Int) : PipelineInfo = ???
    }
    final case class Func2(
      width : Int, leftInfo : PipelineInfo, rightInfo : PipelineInfo, extraStages : Int,
      pipeDelayEstimations: PipeDelayEstimations
    ) extends PipelineInfo {
      val joinedInput = Join(List(leftInfo, rightInfo))
      val leftExtraPipe = joinedInput.extraInputPipe.head
      val rightExtraPipe = joinedInput.extraInputPipe.last
      def pipeRequired : Boolean = extraStages > 0 || leftExtraPipe.pipeRequired || rightExtraPipe.pipeRequired
      val estimation =
        if (pipeDelayEstimations.length > extraStages) pipeDelayEstimations(extraStages)
        else pipeDelayEstimations.last
      val accDelay : Int = joinedInput.accDelay + estimation.produceDelay
      val accPipe : Pipe = joinedInput.accPipe.pipe(extraStages)
      def bitsWL(relWidth : Int, relBitLow : Int) : PipelineInfo = copy(width = relWidth)
    }
    object IfElse {
      def apply(
        width : Int,
        branchInfos : List[(PipelineInfo, PipelineInfo)],
        fallbackInfo : PipelineInfo
      ) : PipelineInfo = ???
    }
  }

  final case class DelayEstimation(consumeDelay : Int, maxDelay : Int, produceDelay : Int)
  type PipeDelayEstimations = Vector[DelayEstimation]
  trait PipeDelayEstimator {
    def func2(func2 : DFAny.Func2)(implicit getSet: MemberGetSet) : PipeDelayEstimations
  }
  implicit object DefaultPipeDelayEstimator extends PipeDelayEstimator {
    private val LUTDelay = 5
    private val LUTInputs = 6
    private val LUTtoLUTPenalty = 1
    private val LUTOutputs = 2
    private val LUTsPerSlice = 4
    private val SliceToSlicePenalty = 5

    private def gateLogicDelay(funcInputWidth : Int, funcOutputWidth : Int, funcDepth : Int, maxPipe : Int)
    : PipeDelayEstimations = {
      val LUTCount = (((funcInputWidth - 1) / LUTInputs) max ((funcOutputWidth - 1) / LUTOutputs)) + 1
      val SliceCount = (LUTCount - 1) / LUTsPerSlice + 1
      val delay = funcDepth * ((LUTCount - 1)*LUTtoLUTPenalty + (SliceCount - 1)*SliceToSlicePenalty + LUTCount * LUTDelay)
      val basicEstimations = Vector(
        DelayEstimation(delay, delay, delay),
        DelayEstimation(delay, delay, 0),
        DelayEstimation(0, delay, 0),
      )
      if (maxPipe <= 2) basicEstimations
      else basicEstimations ++ Vector.tabulate(maxPipe - 2)(i => DelayEstimation(0, delay/(1 << i+1), 0))
    }

    private val noDelay = Vector(DelayEstimation(0, 0, 0))
    def func2(func2 : DFAny.Func2)(implicit getSet : MemberGetSet) : PipeDelayEstimations = {
      val maxWidth = func2.leftArgRef.get.width max func2.rightArgRef.get.width
      val leftIsConstant = func2.leftArgRef.get match { //TODO: include constant propagation analysis
        case _ : DFAny.Const => true
        case _ => false
      }
      val rightIsConstant = func2.rightArgRef.get match { //TODO: include constant propagation analysis
        case _ : DFAny.Const => true
        case _ => false
      }
      if (leftIsConstant && rightIsConstant) noDelay
      else func2.op match {
        case Op.+ | Op.- => gateLogicDelay(maxWidth*2, maxWidth, 1, maxWidth.bitsWidth(false))
        case Op.+^ | Op.-^ => gateLogicDelay((maxWidth+1)*2, maxWidth+1, 1, maxWidth.bitsWidth(false))
        case Op.* => gateLogicDelay(maxWidth*2, maxWidth, maxWidth, maxWidth.bitsWidth(false))
        case Op.*^ => gateLogicDelay((maxWidth+1)*2, maxWidth+1, maxWidth + 1, maxWidth.bitsWidth(false))

        case Op.== | Op.!= => gateLogicDelay(2, 1, maxWidth.bitsWidth(false), 2)
        case Op.< | Op.> | Op.<= | Op.>= => gateLogicDelay(maxWidth*2, maxWidth, 1, 2)
        case Op.| | Op.& | Op.|| | Op.&& | Op.^ => gateLogicDelay(2, 1, 1, 2)
        case Op./ =>
          val isPowerOfTwo = {
            val intValue = func2.rightArgRef.get match {
              case DFAny.Const(_,DFUInt.Token(_,value),_,_) => value
              case DFAny.Const(_,DFSInt.Token(_,value),_,_) => value
              case _ => None
            }
            intValue.exists(v => (v & (v - 1)) == 0)
          }

          if (isPowerOfTwo) noDelay
          else gateLogicDelay(maxWidth*2, maxWidth, maxWidth, maxWidth.bitsWidth(false))
        case shift : Op.Shift =>
          if (rightIsConstant) noDelay else gateLogicDelay(maxWidth*2, maxWidth, maxWidth.bitsWidth(false), 2)
        case Op.++ => noDelay
      }
    }
  }

}
