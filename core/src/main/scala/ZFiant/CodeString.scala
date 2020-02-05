package ZFiant

class CodeString {

}

object CodeString {
  trait Context {
    val callOwner : DFBlock
    val getset : MemberGetSet
  }
  object Context {
    implicit def evContext(implicit ctx : DFMember.Context) : Context = new Context {
      override val callOwner: DFBlock = ctx.owner
      override val getset: MemberGetSet = ctx.db.getset
    }
    implicit def ev(implicit co : DFBlock, gs : MemberGetSet, lp : shapeless.LowPriority) : Context = new Context {
      override val callOwner: DFBlock = co
      override val getset: MemberGetSet = gs
    }
  }

  sealed trait Config
  object Config {
    implicit case object Default extends Config
    case object ShowInits extends Config
  }
}