package dfhdl.compiler.stages
import dfhdl.compiler.ir.*
import dfhdl.internals.*
import dfhdl.options.{CompilerOptions, PrinterOptions}
import wvlet.log.*

import scala.collection.SortedSet
import scala.annotation.tailrec
class StageRunner(using co: CompilerOptions, po: PrinterOptions) extends LogSupport:
  logger.setFormatter(LogFormatter.BareFormatter)
  logger.setLogLevel(co.logLevel)
  private val ignoredTraceStages: Set[Stage] =
    PrintCodeString.dependencies.toSet + PrintCodeString
  def logDebug(): Unit =
    logger.setLogLevel(LogLevel.DEBUG)
  def logInfo(): Unit =
    logger.setLogLevel(LogLevel.INFO)
  def logWarn(): Unit =
    logger.setLogLevel(LogLevel.WARN)
  private def runSingleStage(stage: Stage, done: Set[Stage])(designDB: DB): DB =
    info(s"Running stage ${stage.typeName}....")
    val ret = stage.transform(designDB)(using designDB.getSet)
    info(s"Finished stage ${stage.typeName}")
    if (
      logger.getLogLevel >= LogLevel.DEBUG && !stage.isInstanceOf[SanityCheck] &&
      !stage.isInstanceOf[NoCheckStage]
    )
      if (!done.contains(DropUnreferencedAnons))
        ret.sanityCheck(skipAnonRefCheck = true)
      else
        ret.sanityCheck(skipAnonRefCheck = false)
    if (
      (logger.getLogLevel eq LogLevel.TRACE) &&
      !ignoredTraceStages.contains(stage) && !stage.isInstanceOf[SanityCheck] &&
      !stage.isInstanceOf[SpecialControlStage]
    )
      val ll = logger.getLogLevel
      given CompilerOptions = co.copy(logLevel = LogLevel.OFF)
      if (!ret.equals(designDB))
        ret.printCodeString
      logger.setLogLevel(ll)
    ret
  end runSingleStage
  @tailrec private def run(deps: List[Stage], done: Set[Stage])(
      designDB: DB
  ): DB =
    deps match
      // still have dependencies to run
      case ::(head, next) =>
        // the head stage should not be run, so we move on
        if (!head.runCondition) run(next, done + head)(designDB)
        // the head is already done, so we move on
        else if (done.contains(head)) run(next, done)(designDB)
        // all the dependencies of head are done, so we can run the head stage
        else if ((head.depSet -- done).isEmpty)
          // the stage is done, so we add it to the done set and remove the nullified stages
          val updatedDone = done + head -- head.nullifies
          // running the stage (updatedDone set is given only for tracing purposes)
          val resultDB = runSingleStage(head, updatedDone)(designDB)
          // running the next stage
          run(next, updatedDone)(resultDB)
        // still need to wait for dependencies, so we add them to the deps queue in a sorted order
        // (this is just to preserve consistency in compilation order).
        else run(head.dependencies ++ deps, done)(designDB)
      case Nil => designDB
  def run(stage: Stage)(designDB: DB): DB = run(List(stage), Set())(designDB)
end StageRunner

object StageRunner:
  def run(stage: Stage)(designDB: DB)(using CompilerOptions, PrinterOptions): DB =
    new StageRunner().run(List(stage), Set())(designDB)
