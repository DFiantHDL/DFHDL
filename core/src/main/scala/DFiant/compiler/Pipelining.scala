package DFiant
package compiler

import DFDesign.DB.Patch
import DFiant.compiler.PipelineEvaluator._

final class Pipelining[D <: DFDesign](c : IRCompilation[D]) {
  private val designDB = c.fixAnonymous.db
  private val dc = designDB.dependencyContext
  import designDB.__getset

  abstract class PipeDesign(implicit meta : internals.Meta) extends MetaDesign() {
    def addPipes(member : DFAny.Member, extraPipe : ExtraPipe) : DFAny.Member =
      if (extraPipe.extraStages > 0) member.asVal.pipe[Int](extraPipe.extraStages).anonymize
      else member

    def addPipes(member : DFAny.Member, extraPipes : List[ExtraPipe]) : DFAny.Member = extraPipes match {
      case head :: Nil => addPipes(member, head)
      case Nil => member
      case _  if !member.isAnonymous =>
        val dfVal = member.asVal
        var relBitHigh = member.width - 1
        val pipedParts = extraPipes.map{ep =>
          val relBitLow = relBitHigh - ep.width + 1
          val relBits = dfVal.bitsWL(ep.width, relBitLow).anonymize
          relBitHigh = relBitLow - 1
          addPipes(relBits, ep)
        }
        val piped = pipedParts.map(p => p.asValOf[DFBits.Type[Int]]).reduce(_ ++ _).anonymize
        member.dfType match {
          case DFBits.Type(_) => piped
          case _ => piped.as(DFAny.NewVar(member.dfType)).anonymize
        }
    }
  }

  private def addPipelines(
    pipelineMethod : PipelineMethod, pipeDelayEstimator: PipeDelayEstimator = DefaultPipeDelayEstimator
  ) : IRCompilation[D] = {
    val pipeDelayMap = designDB.evaluate(new PipelineEvaluator(pipelineMethod, pipeDelayEstimator))
    val patchList = pipeDelayMap.flatMap {
      case (SourceValue.Func2(dfVal), PipelineInfo.Func2(width, leftExtraPipes, rightExtraPipes, internalExtraPipe, _, _)) =>
        assert(dfVal.width == width)
        if (leftExtraPipes.pipeRequired || rightExtraPipes.pipeRequired || internalExtraPipe > 0) {
//          println(leftExtraPipes, rightExtraPipes, internalExtraPipe)
          val leftArg = dfVal.leftArgRef.get
          val rightArg = dfVal.rightArgRef.get
          val dsn = new PipeDesign() {
            final val updatedLeftArg = addPipes(leftArg, leftExtraPipes)
            final val updatedRightArg = addPipes(rightArg, rightExtraPipes)
            final val updatedFunc =
              DFAny.Func2.forced(
                dfVal.dfType, updatedLeftArg, dfVal.op, updatedRightArg
              )(dfVal.tokenFunc).anonymize
            addPipes(updatedFunc, List(ExtraPipe(updatedFunc.width, internalExtraPipe))).setTags(_ => dfVal.tags)
          }
          Some(dfVal -> Patch.Add(dsn, Patch.Add.Config.ReplaceWithLast()))
        }
        else None
      case _ => None
    }
    c.newStage(designDB.patch(patchList))
  }

  private def removePipelines : IRCompilation[D] = {
    val patchList = designDB.members.flatMap {
      case dfVal @ DFAny.Alias.Prev(_,relValRef,_,DFAny.Alias.Prev.Pipe,_,_) =>
        Some(dfVal -> Patch.Replace(relValRef.get, Patch.Replace.Config.ChangeRefAndRemove))
      case _ => None
    }
    c.newStage(designDB.patch(patchList))
  }

  def pipeline(
    pipelineMethod : PipelineMethod, pipeDelayEstimator: PipeDelayEstimator = DefaultPipeDelayEstimator
  ) : IRCompilation[D] = {
    pipelineMethod match {
      case PipelineMethod.NoPipeline => removePipelines
      case _ => addPipelines(pipelineMethod, pipeDelayEstimator)
    }
  }
}