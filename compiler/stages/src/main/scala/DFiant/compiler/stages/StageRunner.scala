package DFiant.compiler.stages
import DFiant.compiler.ir.*
import DFiant.internals.*
import wvlet.log.*
import scala.collection.SortedSet
import scala.annotation.tailrec
object StageRunner extends LogSupport:
  Logger.setDefaultFormatter(LogFormatter.AppLogFormatter)
  Logger.setDefaultLogLevel(LogLevel.DEBUG)
  private def runSingleStage(stage: Stage2)(designDB: DB): DB =
    debug(s"Running stage ${stage.typeName}....")
    val ret = stage.run(designDB)
    debug(s"Finished stage ${stage.typeName}")
    ret
  @tailrec def run(deps: List[Stage2], done: Set[Stage2])(
      designDB: DB
  ): DB =
    deps match
      // still have dependencies to run
      case ::(head, next) =>
        // the head stage is already done, so we move on
        if (done.contains(head)) run(next, done)(designDB)
        // all the dependencies of head are done, so we can run the head stage
        else if ((head.depSet -- done).isEmpty)
          // running the stage
          val resultDB = runSingleStage(head)(designDB)
          // the stage is done, so we add it to the done set and remove the nullified stages
          run(next, done + head -- head.nullifies)(resultDB)
        // still need to wait for dependencies, so we add them to the deps queue in a sorted order
        // (this is just to preserve consistency in compilation order).
        else run(head.dependencies ++ deps, done)(designDB)
      case Nil => designDB
  def run(stage: Stage2)(designDB: DB): DB = run(List(stage), Set())(designDB)
end StageRunner
