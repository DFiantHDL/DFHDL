package DFiant
package fsm

import scala.annotation.tailrec
import scala.collection.{immutable, mutable}
import DFiant.internals._

sealed trait FSM
sealed trait FSME extends FSM
object FSM {
  sealed trait Trackable extends FSM {
    protected[fsm] def getEdges : List[FSM]
    protected[fsm] def track : this.type
    protected[fsm] def untrack : this.type
  }
  sealed trait Type
  object Type {
    sealed trait EmptyConn extends Type     //Step or FSM without any outgoing transitions
    sealed trait OnExit extends Type        //Exit block transition
    sealed trait Cond extends Type          //Conditional transition
    sealed trait CondOnExit extends Type    //Conditional + Exit block transition
    sealed trait Branch extends Type        //Branch step / fsm
    sealed trait BranchCond extends Type    //Branch conditional transition
    sealed trait BranchOnExit extends Type  //Branch exit block transition
    sealed trait BranchCondOnExit extends Type  //Branch conditional + exit block transition
    sealed trait BranchDone extends Type    //Branching done (no more outgoing transitions)
  }

  sealed trait Of[T <: Type, R] extends Trackable {
    protected[fsm] def getR : R
    protected[fsm] final def addEdge[O <: Type](fsm : FSM)(implicit ctx : DFAny.Context) : Edges[O, R] = {
      untrack
      Edges[O, R](getEdges :+ fsm)(getR).track
    }
  }
  object Of {
    sealed trait Conn
    sealed trait `==>` extends Conn
    sealed trait `=?>` extends Conn
    sealed trait `=^>` extends Conn
    sealed trait `=!>` extends Conn

    sealed class ConnOp[S <: Type, C <: Conn, O <: Type]

    implicit class `==>Ext`[S <: Type, O <: Type, R](src : Of[S, R])(
      implicit connOp : ConnOp[S, `==>`, O]
    ) {
      def ==>[D <: Type, DR](dst : Of[D, DR])(implicit ctx : DFAny.Context) : Edges[O, DR] = {
        dst.untrack.getEdges.foreach {
          case t : Trackable => t.untrack
          case _ =>
        }
        src.untrack
        Edges[O, DR](src.getEdges ++ (FSM.`==>`(dst.getEdges.head) :: dst.getEdges.drop(1)))(dst.getR).track
      }

      def ==>(dst : => FSM)(implicit ctx : DFAny.Context) : FSM = src.addEdge[O](FSM.`==>`(dst))
    }
    implicit class `=?>Ext`[S <: Type, O <: Type](src : Of[S, Unit])(
      implicit connOp : ConnOp[S, `=?>`, O]
    ) {
      def =?>[C](cond : => C)(implicit arg : => DFBool.Arg[0], ctx : DFAny.Context) : Edges[O, Unit] = src.addEdge[O](FSM.`=?>`(arg()))
    }
    implicit class `=?>ExtR`[S <: Type, O <: Type, R](src : Of[S, R])(
      implicit connOp : ConnOp[S, `=?>`, O]
    ) {
      def =?>[C](cond : R => DFBool)(implicit ctx : DFAny.Context) : Edges[O, R] = src.addEdge[O](FSM.`=?>`(cond(src.getR)))
    }
    implicit class `=^>Ext`[S <: Type, O <: Type](src : Of[S, Unit])(
      implicit connOp : ConnOp[S, `=^>`, O]
    ) {
      def =^>(block : => Unit)(implicit ctx : DFAny.Context) : Edges[O, Unit] = src.addEdge[O](FSM.`=^>`(block))
    }
    implicit class `=^>ExtR`[S <: Type, O <: Type, R](src : Of[S, R])(
      implicit connOp : ConnOp[S, `=^>`, O]
    ) {
      def =^>(block : R => Unit)(implicit ctx : DFAny.Context) : Edges[O, R] = src.addEdge[O](FSM.`=^>`(block(src.getR)))
    }
    implicit class `=!>Ext`[S <: Type, O <: Type, R](src : Of[S, R])(
      implicit connOp : ConnOp[S, `=!>`, O]
    ) {
      def =!>(dst : => FSM)(implicit ctx : DFAny.Context) : Edges[O, R] = src.addEdge[O](FSM.`=!>`(dst))
    }

    import Type._
    implicit val `EmptyConn==>`       = new ConnOp[EmptyConn,       `==>`, EmptyConn]
    implicit val `EmptyConn=?>`       = new ConnOp[EmptyConn,       `=?>`, Cond]
    implicit val `EmptyConn=^>`       = new ConnOp[EmptyConn,       `=^>`, OnExit]
    implicit val `Cond==>`            = new ConnOp[Cond,            `==>`, EmptyConn]
    implicit val `Cond=^>`            = new ConnOp[Cond,            `=^>`, CondOnExit]
    implicit val `Cond=!>`            = new ConnOp[Cond,            `=!>`, Branch]
    implicit val `OnExit==>`          = new ConnOp[OnExit,          `==>`, EmptyConn]
    implicit val `CondOnExit==>`      = new ConnOp[CondOnExit,      `==>`, EmptyConn]
    implicit val `CondOnExit=!>`      = new ConnOp[CondOnExit,      `=!>`, Branch]
    implicit val `Branch=?>`          = new ConnOp[Branch,          `=?>`, BranchCond]
    implicit val `Branch=^>`          = new ConnOp[Branch,          `=^>`, BranchOnExit]
    implicit val `Branch=!>`          = new ConnOp[Branch,          `=!>`, BranchDone]
    implicit val `BranchOnExit=!>`    = new ConnOp[BranchOnExit,    `=!>`, BranchDone]
    implicit val `BranchCond=!>`      = new ConnOp[BranchCond,      `=!>`, Branch]
    implicit val `BranchCond=^>`      = new ConnOp[BranchCond,      `=^>`, BranchCondOnExit]
    implicit val `BranchCondOnExit=!>`= new ConnOp[BranchCondOnExit,`=!>`, Branch]
  }

  sealed trait Step extends FSM {
    val ctx : DFBlock.Context
  }
  final class BasicStep[R](alwaysBlock : () => R)(implicit val ctx : DFBlock.Context) extends Of[Type.EmptyConn, R] with Step {
    private lazy val retVal : R = alwaysBlock()
    def getR : R = retVal
    def getEdges : List[FSM] = List(this)
    protected[fsm] def track : this.type = ctx.db.trackFSM(this)
    protected[fsm] def untrack : this.type = ctx.db.untrackFSM(this)
    override def toString : String = ctx.meta.name
  }
  final case class Edges[T <: Type, R](list : List[FSM])(retVal : => R)(implicit ctx : DFAny.Context) extends Of[T, R] {
    def getR : R = retVal
    def getEdges : List[FSM] = list
    protected[fsm] def track : this.type = ctx.db.trackFSM(this)
    protected[fsm] def untrack : this.type = ctx.db.untrackFSM(this)
  }
  final case class `==>`(dst : () => FSM) extends FSM {
    override def toString : String = "==>"
  }
  final case class `=?>`(dst : () => DFBool) extends FSM {
    override def toString : String = "=?> <cond>"
  }
  final case class `=^>`(dst : () => Unit) extends FSM {
    override def toString : String = "=^> <exit-block>"
  }
  final case class `=!>`(dst : () => FSM) extends FSM {
    override def toString : String = "=!>"
  }


  final case class Transition(cond : Option[() => DFBool], block : Option[() => Unit]) {
    def show(connStr : String) : String = (cond, block) match {
      case (Some(c), Some(b)) => s" =?> ... =^> ... $connStr "
      case (Some(c), None) => s" =?> ... $connStr "
      case (None, Some(b)) => s" =^> ... $connStr "
      case (None, None) => s" $connStr "
    }  
  }
  implicit class GroupByOrderedImplicitImpl[T](val seq: Iterable[T]) extends AnyVal {
    def groupByOrdered[P](f: T => P): Seq[(P, Iterable[T])] = {
      @tailrec
      def accumulator(seq: Iterable[T], f: T => P, res: List[(P, Iterable[T])]): Seq[(P, Iterable[T])] = seq.headOption match {
        case None => res.reverse
        case Some(h) => {
          val key = f(h)
          val subseq = seq.takeWhile(f(_) == key)
          accumulator(seq.drop(subseq.size), f, (key -> subseq) :: res)
        }
      }
      accumulator(seq, f, Nil)
    }
  }
  final case class Elaboration(transitions : immutable.ListMap[Step, immutable.ListSet[(Transition, Step)]]) {
    def addTransition(src : Step, transition : Transition, dst : Step) : Elaboration = {
      val updatedTransitions = transitions.get(src) match {
        case Some(list) => transitions + (src -> (list + Tuple2(transition, dst)))
        case None => transitions + (src -> immutable.ListSet(Tuple2(transition, dst)))
      }
      new Elaboration(updatedTransitions)
    }
    def ++(elaboration : Elaboration) : Elaboration = new Elaboration(elaboration.transitions.foldLeft(this.transitions) {
      case (transitions, addedTransition) => transitions.get(addedTransition._1) match {
        case Some(list) => transitions + (addedTransition._1 -> (list ++ addedTransition._2))
        case None => transitions + addedTransition
      }
    })
    def addStep(src : Step) : Elaboration =
      if (transitions.contains(src)) this
      else new Elaboration(transitions + (src -> immutable.ListSet()))
    def getDestinationStepsOf(src : Step) : Set[Step] = transitions(src).view.map(x => x._2).toSet
    def getDestinationSteps : Set[Step] = transitions.flatMap(x => getDestinationStepsOf(x._1)).toSet
    def getSourcesSteps : Set[Step] = transitions.keys.toSet
    implicit class DistinctStepSets(list : List[Set[Step]]) {
      def addSteps(steps : Set[Step]) : List[Set[Step]] = {
        val partition = list.partition(g => g.intersect(steps).nonEmpty)
        (steps :: partition._1).reduce(_ union _) :: partition._2
      }
    }
    def distinctFSMs : List[Elaboration] = {
      val distinctStepSets = transitions.foldLeft(List.empty[Set[Step]]) {
        case (stepGroups, (step, _)) =>
          stepGroups.addSteps(getDestinationStepsOf(step) + step)
      }
      transitions.groupByOrdered {
        case (step, _) => distinctStepSets.find(group => group.contains(step))
      }.map{e => new Elaboration(immutable.ListMap(e._2.toSeq : _*)).fixOrder}.toList
    }
    def fixOrder : Elaboration = {
      val sourceOnlySteps = getSourcesSteps -- getDestinationSteps
      //too many starting points
      if (sourceOnlySteps.size > 1) throw new IllegalArgumentException(s"Found more than one starting step:\n${sourceOnlySteps.mkString(", ")}")
      sourceOnlySteps.headOption match {
        //found that the starting point isn't first
        case Some(head) if (head != transitions.head._1) =>
          val p = transitions.partition(e => e._1 == head)
          new Elaboration(p._1 ++ p._2)
        //the starting point is first
        case _ => this
      }
    }
    private lazy val steps = transitions.keys
    //returns:             fsmName     stepNames
    private def genNames : (String, Map[Step, String]) = {
      val headName = steps.head.ctx.meta.name.toString
      val nameGroups = steps.zipWithIndex.groupByOrdered(k => k._1.ctx.meta.name.toString)
      val suffixGen = s"%0${steps.size.toString.length}d"
      if (nameGroups.size == 1)
        (headName, steps.zipWithIndex.map{case (s, i) => s -> s"S${suffixGen.format(i)}"}.toMap)
      else
        (headName, nameGroups.flatMap {
          case (name, (head, i) :: Nil) =>
            if (name == headName) Some(head -> s"S${suffixGen.format(i)}")
            else Some(head -> name)
          case (name, steps) =>
            steps.map {case (s, i) => s -> s"${name}_${suffixGen.format(i)}"}
        }.toMap)
    }
    def elaborate : Elaboration = {
      val (fsmName, stepNames) = genNames
      implicit val ctx = steps.head.ctx
      import ctx.db.getSet
      object states extends EnumType.Auto()(ctx.meta.setName(s"${fsmName}_states"))
      val entries : Map[Step, states.Entry] = stepNames.map(e => e._1 -> states.Entry()(ctx.meta.setName(e._2)))
      val state = DFEnum(states) init(entries(steps.head)) setName (s"${fsmName}_state")
      val matchHeader : ConditionalBlock.NoRetVal.HasCaseDF[DFEnum.Type[states.type]] = matchdf(state)
      steps.foldLeft(matchHeader) {
        case (pm, step) => pm.casedf(entries(step)) {
          step match {
            case bs : BasicStep[_] => bs.getR
          }
          transitions(step).toList.foldLeft[Option[ConditionalBlock.NoRetVal.HasElseIfDF]](None) {
            case (None, (t, dst)) => t match {
              case Transition(Some(cond), Some(block)) =>
                Some(ifdf(cond()) {
                  block()
                  state := entries(dst)
                })
              case Transition(Some(cond), None) =>
                Some(ifdf(cond()) {
                  state := entries(dst)
                })
              case Transition(None, Some(block)) =>
                block()
                state := entries(dst)
                None
              case Transition(None, None) =>
                state := entries(dst)
                None
            }
            case (Some(ib), (t, dst)) => t match {
              case Transition(Some(cond), Some(block)) =>
                Some(ib.elseifdf(cond()) {
                  block()
                  state := entries(dst)
                })
              case Transition(Some(cond), None) =>
                Some(ib.elseifdf(cond()) {
                  state := entries(dst)
                })
              case Transition(None, Some(block)) =>
                Some(ib.elsedf {
                  block()
                  state := entries(dst)
                })
                None
              case Transition(None, None) =>
                Some(ib.elsedf {
                  state := entries(dst)
                })
                None
            }
          }
        }
      }
      this
    }

    override def toString : String = transitions.map {
      case (src, l) if l.size == 1 => s"$src${l.head._1.show("==>")}${l.head._2}"
      case (src, l) if l.isEmpty => s"$src [final step]"
      case (src, l) => s"$src${l.map(e => s"${e._1.show("=!>")}${e._2}").mkString(" ")}"
    }.mkString("\n")
  }
  object FirstStep {
    def unapply(arg : Any) : Option[Step] = arg match {
      case Edges(FirstStep(step) :: _) => Some(step)
      case step : Step => Some(step)
      case func : (() => Any) => unapply(func())
      case _ => None
    }
  }
  object LastStep {
    def unapply(arg : Any) : Option[Step] = arg match {
      case Edges(_ :+ LastStep(step)) => Some(step)
      case step : Step => Some(step)
      case func : (() => Any) => unapply(func())
      case _ => None
    }
  }
  object Elaboration {
    def empty : Elaboration = new Elaboration(immutable.ListMap())
    @tailrec private def discover(
      edgeQueue : List[FSM], elaboration : Elaboration
    ) : Elaboration = edgeQueue match {
      case LastStep(src) :: `==>`(FirstStep(dst)) :: edgeList =>
        discover(dst :: edgeList, elaboration.addTransition(src, Transition(None, None), dst))
      case LastStep(src) :: `=?>`(cond) :: `==>`(FirstStep(dst)) :: edgeList =>
        discover(dst :: edgeList, elaboration.addTransition(src, Transition(Some(cond), None), dst))
      case LastStep(src) :: `=^>`(block) :: `==>`(FirstStep(dst)) :: edgeList =>
        discover(dst :: edgeList, elaboration.addTransition(src, Transition(None, Some(block)), dst))
      case LastStep(src) :: `=?>`(cond) :: `=^>`(block) :: `==>`(FirstStep(dst)) :: edgeList =>
        discover(dst :: edgeList, elaboration.addTransition(src, Transition(Some(cond), Some(block)), dst))

      case LastStep(src) :: `=!>`(FirstStep(dst)) :: edgeList =>
        discover(edgeList, elaboration.addTransition(src, Transition(None, None), dst))
      case LastStep(src) :: `=?>`(cond) :: `=!>`(FirstStep(dst)) :: edgeList =>
        discover(src :: edgeList, elaboration.addTransition(src, Transition(Some(cond), None), dst))
      case LastStep(src) :: `=^>`(block) :: `=!>`(FirstStep(dst)) :: edgeList =>
        discover(src :: edgeList, elaboration.addTransition(src, Transition(None, Some(block)), dst))
      case LastStep(src) :: `=?>`(cond) :: `=^>`(block) :: `=!>`(FirstStep(dst)) :: edgeList =>
        discover(src :: edgeList, elaboration.addTransition(src, Transition(Some(cond), Some(block)), dst))

      case FirstStep(src) :: Nil => elaboration.addStep(src)
      case Nil => elaboration
      case _ => throw new IllegalArgumentException(s"Unexpected FSM sequence:\n${edgeQueue.mkString(" ")} <end>")
    }
    def discover(trackable : List[Trackable]) : Elaboration = {
      val elaborations = trackable.map(t => discover(t.getEdges, empty))
      elaborations.reduce(_ ++ _)
    }
    def apply(trackable : List[Trackable]) : Unit = {
      discover(trackable).distinctFSMs.map(f => f.elaborate)
    }
  }
}