package DFiant
package compiler

import DFiant.DFAny.Func2.Op
import DFiant.compiler.csprinter.CSPrinter
import DFiant.internals.IntExtras
import PipelineEvaluator.{Pipe, PipeDelayEstimator, PipelineInfo}


class PipelineEvaluator(
  pipelineMethod: PipelineMethod, delayEstimator: PipeDelayEstimator
) extends ElementEvaluator[PipelineInfo] {
  def applyElement(t : PipelineInfo, element : SourceElement) : PipelineInfo =
    t.bitsWL(element.relWidth, element.relBitLow).pipe(element.pipeStep)

  def apply(element : SourceElement)(
    implicit evaluationMap : EvaluationMap[PipelineInfo], dependencyContext : DependencyContext
  ) : PipelineInfo = {
    import dependencyContext.getSet
    implicit val constEvaluator = ConstEvaluator
    dependencyContext.constMap.evaluate(element) match {
      //a constant (non bubble token value) will always yield no pipeline and no delay
      case token if !token.isBubble => PipelineInfo.Node(element.relWidth, 0, Pipe.NA)
      case _ =>
        val pipeDelayInfo : PipelineInfo = element.srcVal match {
          //new input yields a new pipeline path
          case SourceValue.Dcl(dfVal, _) if dfVal.isTopLevelInput =>
            PipelineInfo.Node(element.relWidth, 0, Pipe.Accumulation(0)).pipe(element.pipeStep)
          case SourceValue.Dcl(dfVal, version) =>
            PipelineInfo.Join(getDependencies(element).map(evaluationMap.evaluate))
          case SourceValue.Func2(func) =>
            val leftJoin = PipelineInfo.Join(func.leftArgRef.getSource.elements.map(evaluationMap.evaluate))
            val rightJoin = PipelineInfo.Join(func.rightArgRef.getSource.elements.map(evaluationMap.evaluate))
            val funcInfo = PipelineInfo.Func2(element.relWidth, leftJoin, rightJoin)
            val manualPipeStages = element.pipeStep
            val estimations = delayEstimator.func2(func)
            val extraPipe = pipelineMethod match {
              case PipelineMethod.NoPipeline => 0 //forced no pipeline
              case _ if manualPipeStages > 0 => 0 //manual stages already set, so no need for "extra" stages
              case PipelineMethod.ManualPipeline => 0 //no manual stages set, but only manual pipeline is possible
              case PipelineMethod.AutoPipeline(targetMaxDelay) => //automatic pipeline
                estimations.indexWhere { e =>
                  e.maxDelay < targetMaxDelay && (e.consumeDelay + funcInfo.accDelay) < targetMaxDelay
                } match {
                  case extraPipe if extraPipe >= 0 => extraPipe
                  case _ => estimations.length
                }
            }
            funcInfo.delayBy(estimations.head.produceDelay).pipe(extraPipe).bitsWL(element.relWidth, element.relBitLow)
          case SourceValue.ApplySel(_, dfVal, idxSrc) => ???
          case SourceValue.Const(_) => ??? //already handled
        }
        if (element.prevStep > 0) pipeDelayInfo.zeroDelay else pipeDelayInfo
    }
  }

  def cyclic(element : SourceElement)(
    implicit evaluationMap : EvaluationMap[PipelineInfo], dependencyContext : DependencyContext
  ) : PipelineInfo = PipelineInfo.Node(element.relWidth, 0, Pipe.Cyclic)

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
    val accDelay : Int
    val accPipe : Pipe
    def zeroDelay : PipelineInfo
    def pipe(extraStages : Int) : PipelineInfo
    def bitsWL(relWidth : Int, relBitLow : Int) : PipelineInfo
  }
  final case class ExtraPipe(width : Int, extraStages : Int) {
    def pipe(extraStages : Int) : ExtraPipe = copy(extraStages = this.extraStages + extraStages)
    def pipe(extraPipe : ExtraPipe) : ExtraPipe = pipe(extraPipe.extraStages)
  }
  implicit class ExtraPipeListOps(list : List[ExtraPipe]) {
    def pipeRequired : Boolean = list.exists(ep => ep.extraStages > 0)
  }
  object PipelineInfo {
    import Pipe._
    final case class Node(width : Int, accDelay : Int, accPipe : Pipe) extends PipelineInfo {left =>
      def pipe(extraStages : Int) : Node =
        copy(accDelay = 0, accPipe = accPipe.pipe(extraStages))
      def delayBy(extraDelay : Int) : Node = copy(accDelay = accDelay + extraDelay)
      def zeroDelay : Node = copy(accDelay = 0)
      def bitsWL(relWidth : Int, relBitLow : Int) : PipelineInfo = copy(width = relWidth)
    }
    final case class Join(paths : List[PipelineInfo]) extends PipelineInfo {
      val width : Int = paths.map(_.width).sum
      val accPipe : Pipe = paths.map(_.accPipe).reduce(_ max _)
      val extraInputPipe : List[ExtraPipe] = {
        accPipe match {
          case Pipe.Cyclic | Pipe.NA => paths.map(p => ExtraPipe(p.width, 0))
          case Accumulation(max) =>
            paths.map(p => p.accPipe match {
              case Pipe.Cyclic | Pipe.NA => ExtraPipe(p.width, 0)
              case Accumulation(stages) => ExtraPipe(p.width, max - stages)
            })
        }
      }
      val accDelay : Int = paths.map(_.accDelay).lazyZip(extraInputPipe).map {
        case (delay, extraPipe) => if (extraPipe.extraStages > 0) 0 else delay
      }.max
      def zeroDelay : PipelineInfo = Node(width = width, accDelay = 0, accPipe = accPipe)
      def pipe(extraStages : Int) : PipelineInfo = Node(width = width, accDelay = 0, accPipe = accPipe.pipe(extraStages))
      def bitsWL(relWidth : Int, relBitLow : Int) : PipelineInfo =
        Node(width = relWidth, accDelay = accDelay, accPipe = accPipe)
    }
    final case class Func2(
      width : Int, leftExtraPipes : List[ExtraPipe], rightExtraPipes : List[ExtraPipe], internalExtraPipe : Int, accDelay : Int, accPipe : Pipe
    ) extends PipelineInfo {
      def pipe(extraStages : Int) : Func2 = {
        val accDelay = if (extraStages > 0) 0 else this.accDelay
        val accPipe = this.accPipe.pipe(extraStages)
        val internalExtraPipe = this.internalExtraPipe + extraStages
        copy(internalExtraPipe = internalExtraPipe, accDelay = accDelay, accPipe = accPipe)
      }

      def delayBy(extraDelay : Int) : Func2 = copy(accDelay = accDelay + extraDelay)
      def zeroDelay : Func2 = copy(accDelay = 0)
      def bitsWL(relWidth : Int, relBitLow : Int) : PipelineInfo = copy(width = relWidth)
    }
    object Func2 {
      def apply(width : Int, leftJoin : Join, rightJoin : Join) : Func2 = {
        val joined = Join(List(leftJoin, rightJoin))
        val leftExtraPipes = leftJoin.extraInputPipe.map(_.pipe(joined.extraInputPipe.head))
        val rightExtraPipes = rightJoin.extraInputPipe.map(_.pipe(joined.extraInputPipe.last))
        Func2(width, leftExtraPipes, rightExtraPipes, 0, joined.accDelay, joined.accPipe)
      }
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
        case shift : Op.Shift =>
          if (rightIsConstant) noDelay else gateLogicDelay(maxWidth*2, maxWidth, maxWidth.bitsWidth(false), 2)
        case Op.++ => noDelay
      }
    }
  }

}
