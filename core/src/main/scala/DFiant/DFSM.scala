package DFiant

import DFiant.internals._

abstract class DFSM()(implicit ctx : DFBlock.Context) {self =>
  final protected implicit val lateConstructionConfig : LateConstructionConfig = LateConstructionConfig.Force(false)
  protected object states extends EnumType.Auto()(ctx.meta.setName(s"${ctx.meta.name}_states"))
  private var _startState : Option[State] = None
  final lazy val state = DFEnum(states) init(_startState.get.entry) setName(s"${ctx.meta.name}_state")
  protected sealed class State private[DFSM](block : () => Unit)(meta : Meta) extends DFSM.AbstractState(block)(meta) {
    private[DFSM] var nextState : Option[State] = None
    final val entry = states.Entry()(meta)
    final def goto()(implicit ctx : DFAny.Context) : Unit = state := entry
  }
  private var currentState : State = _
  final def gotoNext()(implicit ctx : DFAny.Context) : Unit = currentState.nextState match {
    case Some(state) => state.goto()
    case None => throw new IllegalArgumentException("Did not find next state")
  }
  final def gotoStart()(implicit ctx : DFAny.Context) : Unit = _startState.get.goto()
  private var stateList : List[State] = Nil
  object State {
    def apply(block : => Unit)(implicit ctx : DFBlock.Context) : State = {
      val namedMeta =
        if (ctx.meta.namePosition == self.ctx.meta.namePosition || ctx.meta.name.anonymous)
          ctx.meta.setName(s"ST${stateList.length}")
        else ctx.meta
      val state = new State(() => block)(namedMeta)
      if (stateList.isEmpty) _startState = Some(state)
      stateList.headOption.foreach(h => h.nextState = Some(state))
      stateList = state :: stateList
      state
    }
    def doWhile[C](cond : DFBool.Op.Able[C])(block : => Unit)(
      implicit ctx : DFBlock.Context, condConv : DFBool.`Op:=`.Builder[DFBool.Type, C]
    ) : State = {
      def controlBlock = {
        ConditionalBlock.NoRetVal.IfBlock(condConv(DFBool.Type(logical = true),cond))(block)(ctx)
          .elsedf(gotoNext())
      }
      State(controlBlock)
    }
    def doUntil[C](cond : DFBool.Op.Able[C])(block : => Unit)(
      implicit ctx : DFBlock.Context, condConv : DFBool.`Op:=`.Builder[DFBool.Type, C]
    ) : State = {
      def controlBlock = {
        ConditionalBlock.NoRetVal.IfBlock(condConv(DFBool.Type(logical = true),cond))(gotoNext())(ctx)
          .elsedf(block)
      }
      State(controlBlock)
    }
    def doNext(block : => Unit)(implicit ctx : DFBlock.Context) : State = {
      def execBlock = {
        block
        gotoNext()
      }
      State(execBlock)
    }
    def waitWhile[C](cond : DFBool.Op.Able[C])(
      implicit ctx : DFBlock.Context, condConv : DFBool.`Op:=`.Builder[DFBool.Type, C]
    ) : State = doWhile(cond)({})
    def waitUntil[C](cond : DFBool.Op.Able[C])(
      implicit ctx : DFBlock.Context, condConv : DFBool.`Op:=`.Builder[DFBool.Type, C]
    ) : State = doUntil(cond)({})
    def waitForever(implicit ctx : DFBlock.Context) : State = State({})
    def next(implicit ctx : DFBlock.Context) : State = State(gotoNext())
  }
  private lazy val constructMatchStatement : Unit = {
    val matchHeader = ConditionalBlock.NoRetVal.MatchHeader(state, MatchConfig.NoOverlappingCases)
    currentState = _startState.get
    val matcherFirstCase = matchHeader.casedf(_startState.get.entry)(_startState.get.block())
    stateList.dropRight(1).foldRight(matcherFirstCase)((state, lastCase) => {
      currentState = state
      lastCase.casedf(state.entry)(state.block())
    })
  }
  protected def startAt(startState : State) : this.type = {
    _startState = Some(startState)
    constructMatchStatement
    this
  }
  protected def start() : this.type = startAt(_startState.get)
}
object DFSM {
  protected sealed abstract class AbstractState (val block : () => Unit)(meta : Meta) {
    val entry : EnumType.Entry
    def goto()(implicit ctx : DFAny.Context) : Unit
    final override def toString : String = meta.name
  }
  implicit class DFSM_Ext[F <: DFSM](f : F) {
    def start() : F = f.start()
    def startAt(stateSel : F => F#State) : F = f.startAt(stateSel(f).asInstanceOf[f.State])
  }
}
