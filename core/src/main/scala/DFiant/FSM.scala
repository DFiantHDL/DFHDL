package DFiant

import DFiant.FSM.Step
import DFiant.internals.{GroupByOrderedImplicitImpl, IterableStringsOps}
import DFDesign.Frontend._

import scala.collection.immutable.ListMap
sealed abstract class FSM(implicit protected[FSM] val ctx : DFBlock.Context) {
  final protected[FSM] def track : this.type = ctx.db.trackFSM(this)
  final protected[FSM] def untrack : this.type = ctx.db.untrackFSM(this)
  protected[DFiant] def getStepMap : ListMap[Step, Option[Step]]
  protected def getHeadStep : Step
  /**
    * Destination Step Edge Connection
    *
    * Adds a connection from the LHS last step to the RHS FSM beginner step.
    * @param dstFSM The destination FSM
    * @return a new connected FSM
    */
  final def ==> (dstFSM : => FSM)(
    implicit ctx : DFBlock.Context, ta : FSM.HasFSMAscription
  ) : FSM = {
    untrack
    new FSM.StepChain(getStepMap, () => dstFSM).track
  }
  /**
    * Destination Step Edge Connection
    *
    * Constructs the RHS into an FSM and adds a connection from the LHS last step to the RHS step.
    * @param dstBlock A block of statements to become the body the first step in the RHS FSM.
    * @return a new connected FSM
    */
  final def ==> [T <: FSM.Capable](dstBlock : => T)(
    implicit ctx : DFBlock.Context, ta : FSM.HasFSMAscription, di : DummyImplicit
  ) : FSM = this ==> FSM(dstBlock)

  /**
    * Sets an exit edge from the current step where this statement is executed to this step
    */
  def goto() : Unit

  /**
    * Exit Block Edge Connection
    *
    * Adds an exit block edge at the last FSM step. The exit block is activated during exit from the step.
    * The exit block statements must be followed by a connecting edge `==>` to set the destination step.
    * @param exitBlock the exit statement block
    * @return a new FSM with a dangling exit block edge
    */
  def =^> (exitBlock : => Unit)(
    implicit ctx : DFBlock.Context, ta : FSM.HasFSMAscription
  ) : FSM
}

object FSM {
  trait Capable
  protected[DFiant] final class Step private[FSM] (val alwaysBlock : () => Unit, val exitBlock : () => Unit = () => {})(
    implicit ctx : DFBlock.Context
  ) extends FSM {
    protected[DFiant] lazy val getStepMap : ListMap[Step, Option[Step]] = ListMap(this -> None)
    protected def getHeadStep : Step = this
    private var dstSteps : Set[Step] = Set()
    private var gotoStepFunc : Step => Unit = _ => {}
    protected[FSM] def getDstSteps : Set[Step] = dstSteps
    protected[FSM] def addDstStep(dstStep : Step) : Unit = dstSteps = dstSteps + dstStep
    protected[FSM] def injectGotoFunc(f : Step => Unit) : Unit = gotoStepFunc = f
    def goto() : Unit = gotoStepFunc(this)
    def =^> (exitBlock : => Unit)(
      implicit ctx : DFBlock.Context, ta : FSM.HasFSMAscription
    ) : FSM = {
      untrack
      new Step(alwaysBlock, () => {this.exitBlock(); exitBlock})
    }
    override def toString : String = ctx.meta.name
  }
  protected[FSM] final class StepChain private[FSM] (stepMap : ListMap[Step, Option[Step]], nextFSM : () => FSM)(
    implicit ctx : DFBlock.Context
  ) extends FSM {
    protected[DFiant] lazy val getStepMap : ListMap[Step, Option[Step]] = {
      val nextFSMStrict = nextFSM()
      val nextStep = nextFSMStrict.getHeadStep
      val stepMapWithNextStep = stepMap.updated(stepMap.last._1, Some(nextStep))
      nextFSMStrict match {
        case step : Step =>
          step.untrack
          stepMapWithNextStep.updated(nextStep, None)
        case _ : StepChain =>
          stepMapWithNextStep
      }
    }
    protected def getHeadStep : Step = stepMap.head._1
    def goto() : Unit = getHeadStep.goto()
    def =^> (exitBlock : => Unit)(
      implicit ctx : DFBlock.Context, ta : FSM.HasFSMAscription
    ) : FSM = {
      untrack
      val updatedStep = getStepMap.last._1.=^>(exitBlock).asInstanceOf[Step]
      val stepMapRemovedLast = getStepMap.dropRight(1)
      val updatedStepMap =
        stepMapRemovedLast.updated(stepMapRemovedLast.last._1, Some(updatedStep)) + (updatedStep -> None)
      new StepChain(updatedStepMap, () => updatedStep)
    }
  }

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Implicit that checks if the owner of a term has a type signature.
  // If no type ascription is found a compile error is generated with instructions how to fix.
  // Type ascription must be used when FSM are cyclic.
  // This implicit helps us force consistency for all named FSM declarations,
  // regardless if they are cyclic or not, and remove confusion from users that
  // may trip cyclic errors since they are more confusing in this context.
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  trait HasFSMAscription
  object HasFSMAscription {
    import scala.reflect.macros.blackbox
    implicit def ev : HasFSMAscription = macro evMacro
    def evMacro(c: blackbox.Context) : c.Tree = {
      import c.universe._
      val ownerTerm = c.internal.enclosingOwner.asTerm
      try {
        ownerTerm.typeSignature //will cause a (cyclic) exception if there no type ascription
      } catch {
        case _ : Throwable =>
          c.abort(c.enclosingPosition,
            s"""All named FSMs must have the type ascription `FSM`.
              |Change `val ${ownerTerm.name} = ...` to `val ${ownerTerm.name} : FSM = ...`""".stripMargin
          )
      }
      q"new DFiant.FSM.HasFSMAscription{}"
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////

  def apply[T](alwaysBlock : => T)(
    implicit ctx : DFBlock.Context, ta : HasFSMAscription
  ) : FSM = new Step(() => alwaysBlock).track

  private def prevStep(implicit ctx : DFAny.Context) : FSM.Step = ctx.db.getPrevFSMStep match {
    case Some(value) => value
    case None => ???
  }

  private implicit class DistinctStepSets(list : List[Set[Step]]) {
    def addSteps(steps : Set[Step]) : List[Set[Step]] = {
      val partition = list.partition(g => g.intersect(steps).nonEmpty)
      (steps :: partition._1).reduce(_ union _) :: partition._2
    }
  }
  private implicit class StepMapOps(list : List[(Step, Set[Step])]) {
    def getSourcesSteps : Set[Step] = list.map(_._1).toSet
    def getDestinationSteps : Set[Step] = list.flatMap(_._2).toSet
    def fixOrder : List[(Step, Set[Step])] = {
      val sourceOnlySteps = getSourcesSteps -- getDestinationSteps
      //too many starting points
      if (sourceOnlySteps.size > 1) throw new IllegalArgumentException(s"Found more than one starting step:\n${sourceOnlySteps.mkString(", ")}")
      sourceOnlySteps.headOption match {
        //found that the starting point isn't first
        case Some(head) if (head != list.head._1) =>
          val p = list.partition(e => e._1 == head)
          p._1 ++ p._2
        //the starting point is first
        case _ => list
      }
    }
  }
  private implicit class FSMTableOps(table : ListMap[Step, Set[Step]]) {
    val steps : Iterable[Step] = table.keys
    //returns:             fsmName     stepNames
    def genNames : (String, ListMap[Step, String]) = {
      val prefix = "(.*)_(.*)".r
      val headName = steps.map(s => s.ctx.meta.name.toString).longestCommmonPrefix match {
        case prefix(s,_) if s.nonEmpty => s
        case _ => "fsm"
      }
      val nameGroups = steps.zipWithIndex.groupByOrdered(k => k._1.ctx.meta.name.toString.stripPrefix(s"${headName}_"))

      val suffixGen = s"%0${steps.size.toString.length}d"
      if (nameGroups.size == 1)
        (headName, ListMap.from(steps.zipWithIndex.map{case (s, i) => s -> s"S${suffixGen.format(i)}"}))
      else
        (headName, ListMap.from(nameGroups.flatMap {
          case (name, (head, i) :: Nil) =>
            if (name.isEmpty) Some(head -> s"S${suffixGen.format(i)}")
            else Some(head -> name)
          case (name, steps) =>
            steps.map {case (s, i) => s -> s"${name}_${suffixGen.format(i)}"}
        }))
    }
  }
  protected[DFiant] def elaboration(mutableDB : DFDesign.DB.Mutable, fsmTrack : List[FSM]) : Unit = {
    //We need to elaborate the FSM blocks to know the step connectivity.
    //Since this elaboration mutates the db, we save it and restore it when we're don.
    val checkPoint = mutableDB.saveCheckPoint
    //currentStep will hold the step when we do elaborate all the FSM steps.
    var currentStep : Step = null
//    println("fsmTrack", fsmTrack)
    //Injecting all the FSM steps goto calls with a call function to add the destination step
    fsmTrack.foreach(f => f.getStepMap.foreach{case (step,_) =>
      step.injectGotoFunc(s => currentStep.addDstStep(s))
    })
    //Elaborating all the steps to get their destination edges.
    val nextStepMap = fsmTrack.flatMap(f => f.getStepMap.map{case (step, nextStep) =>
      currentStep = step
      val savedNS = mutableDB.getNextFSMStep
      mutableDB.setNextFSMStep(nextStep)
      step.alwaysBlock()
      mutableDB.setNextFSMStep(savedNS)
      nextStep match {
        //A nextStep connection is allowed if the current step has an explicit nextStep.goto()
        //or if it has no goto calls
        case Some(ns) if !step.getDstSteps.contains(ns) && step.getDstSteps.nonEmpty =>
          throw new IllegalArgumentException(s"\nFSM step $step has a nextStep `==>` connection without an internal `nextStep.goto`")
        //There is a nextStep connection and no goto calls, so we assume an implicit goto call
        //and add the nextStep as a destination. Later in code we will handle this special case
        //when elaborating the FSM.
        case Some(ns) =>
          val addGotoNext = step.getDstSteps.isEmpty
          step.addDstStep(ns)
          (step, (nextStep, addGotoNext))
        case _ =>
          (step, (nextStep, false))
      }
    }).toMap
    //restoring the database, since we want the entire FSM properly elaborated inside as a case statement
    mutableDB.restoreCheckPoint(checkPoint)
//    println(fsmTrack.map(_.getStepMap.map(_._1.getDstSteps)))

    val mixedSteps : List[(Step, Set[Step])] = fsmTrack.flatMap(_.getStepMap.keys.map(k => (k, k.getDstSteps)))
//    println("mixedSteps", mixedSteps)
    val distinctStepSets = mixedSteps.foldLeft(List.empty[Set[Step]]) {
      case (stepGroups, (src, dstSteps)) => stepGroups.addSteps(dstSteps + src)
    }
//    println("distinctStepSets", distinctStepSets)
    val fsmTables : Seq[ListMap[Step, Set[Step]]] = mixedSteps.groupByOrdered {
      case (step, _) => distinctStepSets.find(group => group.contains(step))
    }.map(e => ListMap.from(e._2.toList.fixOrder))//.map{e => ListMap(e._2.toSeq : _*)).fixOrder}.toList
//    println("fsmTables", fsmTables)

    fsmTables.foreach { fsmTable =>
      val (fsmName, stepNames) = fsmTable.genNames
      val steps = fsmTable.steps
      implicit val ctx = steps.head.ctx
      import DFDesign.Frontend.__DFEnumTokenEntry
      import ctx.db.getSet
      object states extends DFEnum.Auto()(ctx.meta.setName(s"${fsmName}_states"))
      val entries : ListMap[Step, states.Entry] = stepNames.map(e => e._1 -> states.Entry()(ctx.meta.setName(e._2)))
      val state = DFEnum(states) <> VAR init(entries(steps.head)) setName (s"${fsmName}_state")
      mutableDB.setPrevFSMStep(Some(steps.head))
      //injecting state change for goto calls
      steps.foreach(_.injectGotoFunc { ns =>
        prevStep.exitBlock()
        state := entries(ns)
      })
      val matchHeader = matchdf(state)
      steps.foldLeft[DFConditional.NoRetVal.HasCaseDF[DFEnum.Type[states.type], true]](matchHeader) {
        case (pm, step) => pm.casedf(entries(step)) {
          val scope = new DFScope(Some(s"${fsmName}_${entries(step).name}")) {
            __db.OwnershipContext.injectContainer(this)
            val savedNS = mutableDB.getNextFSMStep
            val savedPS = mutableDB.getPrevFSMStep
            mutableDB.setPrevFSMStep(Some(step))
            val (nextStep, addGotoNext) = nextStepMap(step)
            mutableDB.setNextFSMStep(nextStep)
            //elaborate fsm step content
            step.alwaysBlock()
            //add nextStep.goto that is implicit in a block without goto statement, but has a nextStep connection
            if (addGotoNext) nextStep.get.goto()
            mutableDB.setPrevFSMStep(savedPS)
            mutableDB.setNextFSMStep(savedNS)
            __db.OwnershipContext.clearInjectedContainer()
          }
        }
      }

    }
  }
}
