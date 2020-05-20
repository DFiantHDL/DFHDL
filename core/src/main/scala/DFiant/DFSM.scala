package DFiant

import DFiant.internals._

abstract class DFSM(implicit ctx : DFBlock.Context) extends DFSM.Abstract {self =>
  private[DFiant] final lazy val __ctx : DFBlock.Context = ctx
}

object DFSM {
  trait Abstract extends DFOwner.Container {self =>
    type Owner = DFSM.Owner
    private[DFiant] val __ctx : DFBlock.Context
    private[DFiant] final val owner : Owner = DFSM.Owner()(__ctx)
    private[DFiant] final val ownerInjector : DFMember.OwnerInjector = new DFMember.OwnerInjector(owner)
    private[DFiant] final lazy val __db: DFDesign.DB.Mutable = __ctx.db
    final protected implicit val __lateConstructionConfig : LateConstructionConfig = LateConstructionConfig.Force(false)

    final protected implicit def __blockContext(
      implicit meta : Meta
    ) : DFBlock.Context = new DFBlock.Context(meta, ownerInjector, ASIS, __db, ClassArgs.empty)

    protected object __dev {
      object states extends EnumType.Auto()
      var startState : Option[State] = None
      final lazy val state = DFEnum(states) init(startState.get.entry)
      var currentState : State = _
      var stateList : List[State] = Nil
    }
    import __dev._

    protected sealed class State private[DFSM](block : () => Unit)(meta : Meta) extends DFSM.AbstractState(block)(meta) {
      private[DFSM] var nextState : Option[State] = None
      final val entry = states.Entry()(meta)
      final def goto()(implicit __blockContext : DFBlock.Context) : Unit = state := entry
    }

    /////////////////////////////////////////////////////////////////////////////////////////////////////
    // Public API
    /////////////////////////////////////////////////////////////////////////////////////////////////////
    protected def gotoNext()(implicit __blockContext : DFBlock.Context) : Unit = currentState.nextState match {
      case Some(state) => state.goto()
      case None => throw new IllegalArgumentException("Did not find next state")
    }
    protected def gotoStart()(implicit __blockContext : DFBlock.Context) : Unit = startState.get.goto()
    object State {
      def apply(block : => Unit)(implicit __blockContext : DFBlock.Context) : State = {
        val namedMeta =
          if (__blockContext.meta.namePosition == __ctx.meta.namePosition || __blockContext.meta.name.anonymous)
            __blockContext.meta.setName(s"ST${stateList.length}")
          else __blockContext.meta
        val state = new State(() => block)(namedMeta)
        if (stateList.isEmpty) startState = Some(state)
        stateList.headOption.foreach(h => h.nextState = Some(state))
        stateList = state :: stateList
        state
      }
    }
    protected def doWhile[C](cond : DFBool.Op.Able[C])(block : => Unit)(
      implicit __blockContext : DFBlock.Context, condConv : DFBool.`Op:=`.Builder[DFBool.Type, C]
    ) : State = {
      def controlBlock = {
        ConditionalBlock.NoRetVal.IfBlock(condConv(DFBool.Type(logical = true),cond))(block)(__blockContext)
          .elsedf(gotoNext())
      }
      State(controlBlock)
    }
    protected def doUntil[C](cond : DFBool.Op.Able[C])(block : => Unit)(
      implicit __blockContext : DFBlock.Context, condConv : DFBool.`Op:=`.Builder[DFBool.Type, C]
    ) : State = {
      def controlBlock = {
        ConditionalBlock.NoRetVal.IfBlock(condConv(DFBool.Type(logical = true),cond))(gotoNext())(__blockContext)
          .elsedf(block)
      }
      State(controlBlock)
    }
    protected def step(block : => Unit)(implicit __blockContext : DFBlock.Context) : State = {
      def execBlock = {
        block
        gotoNext()
      }
      State(execBlock)
    }
    protected def last(block : => Unit)(implicit __blockContext : DFBlock.Context) : State = {
      def execBlock = {
        block
        gotoNext()
      }
      State(execBlock)
    }
    protected def waitWhile[C](cond : DFBool.Op.Able[C])(
      implicit __blockContext : DFBlock.Context, condConv : DFBool.`Op:=`.Builder[DFBool.Type, C]
    ) : State = doWhile(cond)({})
    protected def waitUntil[C](cond : DFBool.Op.Able[C])(
      implicit __blockContext : DFBlock.Context, condConv : DFBool.`Op:=`.Builder[DFBool.Type, C]
    ) : State = doUntil(cond)({})
    protected def waitForever(implicit __blockContext : DFBlock.Context) : State = State({})
    protected def next(implicit __blockContext : DFBlock.Context) : State = State(gotoNext())
    protected def done(implicit __blockContext : DFBlock.Context) : State = last({})
    protected def isDone(implicit __blockContext : DFBlock.Context) : DFBool = ???

    private lazy val constructMatchStatement : Unit = {
      val matchHeader = ConditionalBlock.NoRetVal.MatchHeader(state, MatchConfig.NoOverlappingCases)
      currentState = startState.get
      val matcherFirstCase = matchHeader.casedf(startState.get.entry)(startState.get.block())
      stateList.dropRight(1).foldRight(matcherFirstCase)((state, lastCase) => {
        currentState = state
        lastCase.casedf(state.entry)(state.block())
      })
    }
    protected def startAt(startState : State) : this.type = {
      __dev.startState = Some(startState)
      constructMatchStatement
      this
    }
    protected def start() : this.type = startAt(startState.get)
  }

  protected sealed abstract class AbstractState (val block : () => Unit)(meta : Meta) {
    val entry : EnumType.Entry
    def goto()(implicit __blockContext : DFBlock.Context) : Unit
    final override def toString : String = meta.name
  }
  implicit class DFSM_Ext[F <: DFSM](f : F) {
    def start() : F = f.start()
    def startAt(stateSel : F => F#State) : F = f.startAt(stateSel(f).asInstanceOf[f.State])
  }

  final case class Owner(
    ownerRef : DFOwner.Ref, tags : DFMember.Tags.Basic
  ) extends DFOwner.NameFlattenOwner {
    type TTags = DFMember.Tags.Basic
    type TCustomTag = DFMember.CustomTag
    val nameFlatten: DFOwner.NameFlatten = DFOwner.NameFlatten.UnderscoreSuffix
    protected[DFiant] def =~(that : DFMember)(implicit getSet : MemberGetSet) : Boolean = that match {
      case Owner(_, tags) => this.tags =~ tags
      case _ => false
    }
    def setTags(tagsFunc : DFMember.Tags.Basic => DFMember.Tags.Basic)(
      implicit getSet : MemberGetSet
    ) : DFMember = getSet.set(this)(m => m.copy(tags = tagsFunc(m.tags)))
  }
  object Owner {
    def apply()(
      implicit ctx : DFAny.Context
    ) : Owner = ctx.db.addMember(Owner(ctx.owner, ctx.meta))
  }

}
