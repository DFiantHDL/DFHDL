package DFiant
package compiler

import DFDesign.DB.Patch
import DFiant.compiler.PipelineEvaluator._

final class Pipelining[D <: DFDesign](c: IRCompilation[D]) {
  private val designDB = c.fixAnonymous.db
  private val dc       = designDB.dependencyContext
  import designDB.__getset

  abstract class PipeDesign(implicit meta : internals.Meta) extends MetaDesign() {
    def addPipes(member : DFAny.Member, extraStages : Int) : DFAny.Member =
      if (extraStages > 0) member.asVal.pipe[Int](extraStages).anonymize
      else member

//    def addPipes(member : DFAny.Member, joinedPath : PipelineInfo.Join) : DFAny.Member = joinedPath.extraInputPipe match {
//      case head :: Nil => addPipes(member, head)
//      case Nil => member
//      case extraPipes if !member.isAnonymous =>
//        val dfVal = member.asVal
//        var relBitHigh = member.width - 1
//        val pipedParts = extraPipes.map{ep =>
//          val relBitLow = relBitHigh - ep.width + 1
//          val relBits = dfVal.bitsWL(ep.width, relBitLow).anonymize
//          relBitHigh = relBitLow - 1
//          addPipes(relBits, ep)
//        }
//        val piped = pipedParts.map(p => p.asValOf[DFBits.Type[Int]]).reduce(_ ++ _).anonymize
//        member.dfType match {
//          case DFBits.Type(_) => piped
//          case _ => piped.as(member.dfType).anonymize
//        }
//      case _ => ???
//    }
  }

  private def addPipelines(
    pipelineMethod : PipelineMethod, pipeDelayEstimator: PipeDelayEstimator = DefaultPipeDelayEstimator
  ) : IRCompilation[D] = {
    val pipeDelayMap = designDB.evaluate(new PipelineEvaluator(pipelineMethod, pipeDelayEstimator))
    val manualPipeFuncSet = pipeDelayMap.view.keys.collect {
      case SourceValue(DFAny.Alias.Prev.Unref(_,func : DFAny.Func2,_,DFAny.Alias.Prev.Pipe,_,_), _) => func
    }.toSet
    val patchList = pipeDelayMap.flatMap {
      case (SourceValue(func : DFAny.Func2, _), funcInfo : PipelineInfo.Func2)
        if !manualPipeFuncSet.contains(func) && funcInfo.pipeRequired =>
        assert(func.width == funcInfo.width)
//          println(leftExtraPipes, rightExtraPipes, internalExtraPipe)
        val leftArg = func.leftArgRef.get
        val rightArg = func.rightArgRef.get
        val dsn = new PipeDesign() {
          final val updatedLeftArg = addPipes(leftArg, funcInfo.leftExtraPipe.extraStages)
          final val updatedRightArg = addPipes(rightArg, funcInfo.rightExtraPipe.extraStages)
          final val updatedFunc =
            DFAny.Func2.forced(
              func.dfType, updatedLeftArg, func.op, updatedRightArg
            )(func.tokenFunc).anonymize
          addPipes(updatedFunc, funcInfo.extraStages).setTags(_ => func.tags)
        }
        Some(func -> Patch.Add(dsn, Patch.Add.Config.ReplaceWithLast()))
      case _ => None
    }
    c.newStage(designDB.patch(patchList))
  }

  private def removePipelines: IRCompilation[D] = {
    val patchList = designDB.members.flatMap {
      case dfVal @ DFAny.Alias.Prev(
            _,
            relValRef,
            _,
            DFAny.Alias.Prev.Pipe,
            _,
            _
          ) =>
        Some(
          dfVal -> Patch
            .Replace(relValRef.get, Patch.Replace.Config.ChangeRefAndRemove)
        )
      case _ => None
    }
    c.newStage(designDB.patch(patchList))
  }

  def pipeline(
      pipelineMethod: PipelineMethod,
      pipeDelayEstimator: PipeDelayEstimator = DefaultPipeDelayEstimator
  ): IRCompilation[D] = {
    pipelineMethod match {
      case PipelineMethod.NoPipeline => removePipelines
      case _                         => addPipelines(pipelineMethod, pipeDelayEstimator)
    }
  }
}
