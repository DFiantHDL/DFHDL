package DFiant

import DFiant.internals._

abstract class DFSM()(implicit ctx : DFBlock.Context) {self =>
  final protected implicit val __lateConstructionConfig : LateConstructionConfig = LateConstructionConfig.Force(false)
  protected object __states extends EnumType.Auto()(ctx.meta.setName(s"${ctx.meta.name}_states"))
  private var __startState : Option[State] = None
  protected final lazy val __state = DFEnum(__states) init(__startState.get.entry) setName(s"${ctx.meta.name}_state")
  protected sealed class State private[DFSM](block : () => Unit)(meta : Meta) extends DFSM.AbstractState(block)(meta) {
    private[DFSM] var nextState : Option[State] = None
    final val entry = __states.Entry()(meta)
    final def goto()(implicit ctx : DFAny.Context) : Unit = __state := entry
  }
  private var __currentState : State = _
  private var stateList : List[State] = Nil

  /////////////////////////////////////////////////////////////////////////////////////////////////////
  // Public API
  /////////////////////////////////////////////////////////////////////////////////////////////////////
  protected def gotoNext()(implicit ctx : DFAny.Context) : Unit = __currentState.nextState match {
    case Some(state) => state.goto()
    case None => throw new IllegalArgumentException("Did not find next state")
  }
  protected def gotoStart()(implicit ctx : DFAny.Context) : Unit = __startState.get.goto()
  object State {
    def apply(block : => Unit)(implicit ctx : DFBlock.Context) : State = {
      val namedMeta =
        if (ctx.meta.namePosition == self.ctx.meta.namePosition || ctx.meta.name.anonymous)
          ctx.meta.setName(s"ST${stateList.length}")
        else ctx.meta
      val state = new State(() => block)(namedMeta)
      if (stateList.isEmpty) __startState = Some(state)
      stateList.headOption.foreach(h => h.nextState = Some(state))
      stateList = state :: stateList
      state
    }
  }
  protected def doWhile[C](cond : DFBool.Op.Able[C])(block : => Unit)(
    implicit ctx : DFBlock.Context, condConv : DFBool.`Op:=`.Builder[DFBool.Type, C]
  ) : State = {
    def controlBlock = {
      ConditionalBlock.NoRetVal.IfBlock(condConv(DFBool.Type(logical = true),cond))(block)(ctx)
        .elsedf(gotoNext())
    }
    State(controlBlock)
  }
  protected def doUntil[C](cond : DFBool.Op.Able[C])(block : => Unit)(
    implicit ctx : DFBlock.Context, condConv : DFBool.`Op:=`.Builder[DFBool.Type, C]
  ) : State = {
    def controlBlock = {
      ConditionalBlock.NoRetVal.IfBlock(condConv(DFBool.Type(logical = true),cond))(gotoNext())(ctx)
        .elsedf(block)
    }
    State(controlBlock)
  }
  protected def step(block : => Unit)(implicit ctx : DFBlock.Context) : State = {
    def execBlock = {
      block
      gotoNext()
    }
    State(execBlock)
  }
  protected def last(block : => Unit)(implicit ctx : DFBlock.Context) : State = {
    def execBlock = {
      block
      gotoNext()
    }
    State(execBlock)
  }
  protected def waitWhile[C](cond : DFBool.Op.Able[C])(
    implicit ctx : DFBlock.Context, condConv : DFBool.`Op:=`.Builder[DFBool.Type, C]
  ) : State = doWhile(cond)({})
  protected def waitUntil[C](cond : DFBool.Op.Able[C])(
    implicit ctx : DFBlock.Context, condConv : DFBool.`Op:=`.Builder[DFBool.Type, C]
  ) : State = doUntil(cond)({})
  protected def waitForever(implicit ctx : DFBlock.Context) : State = State({})
  protected def next(implicit ctx : DFBlock.Context) : State = State(gotoNext())
  protected def done(implicit ctx : DFBlock.Context) : State = last({})
  protected def isDone(implicit ctx : DFBlock.Context) : DFBool = ???

  private lazy val constructMatchStatement : Unit = {
    val matchHeader = ConditionalBlock.NoRetVal.MatchHeader(__state, MatchConfig.NoOverlappingCases)
    __currentState = __startState.get
    val matcherFirstCase = matchHeader.casedf(__startState.get.entry)(__startState.get.block())
    stateList.dropRight(1).foldRight(matcherFirstCase)((state, lastCase) => {
      __currentState = state
      lastCase.casedf(state.entry)(state.block())
    })
  }
  protected def startAt(startState : State) : this.type = {
    __startState = Some(startState)
    constructMatchStatement
    this
  }
  protected def start() : this.type = startAt(__startState.get)
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
