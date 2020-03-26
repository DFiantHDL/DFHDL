package ZFiant

import ZFiant.compiler.printer.Printer

sealed trait DFSimMember extends DFMember {
  type TTags = DFMember.Tags.Basic
}
object DFSimMember {
//  protected case class Assert(cond : Option[DFAny], msg : DFString, severity : Severity)(implicit ctx0 : DFAny.Op.Context) extends DFAnySimMember {
//    final private[DFiant] override lazy val ctx = ctx0
//    protected[DFiant] trait __DevAssert extends __DevDFAnyMember {
//      /////////////////////////////////////////////////////////////////////////////////////////////////////////
//      // Member discovery
//      /////////////////////////////////////////////////////////////////////////////////////////////////////////
//      @inline override private[DFiant] def discoveryDependenciesStatic : Set[DFAnyMember] =
//        super.discoveryDependenciesStatic ++ cond.toList
//
//      final val condVersionedSource = cond.map(c => c.source.versioned)
//
//      /////////////////////////////////////////////////////////////////////////////////////////////////////////
//      // Naming
//      /////////////////////////////////////////////////////////////////////////////////////////////////////////
//      override lazy val nameScala = s"${Meta.Name.Separator}assert"
//      def codeString : String = cond match {
//        case Some(c) =>
//          s"""
//             |sim.assert(${c.refCodeString}, ${msg.codeString}, ${severity.codeString})""".stripMargin
//        case None =>
//          s"""
//             |sim.report(${msg.codeString}, ${severity.codeString})""".stripMargin
//      }
//    }
//    override private[DFiant] lazy val __dev : __DevAssert = new __DevAssert {}
//    import __dev._
//    id
//  }

  sealed trait Severity extends Product with Serializable
  object Severity {
    case object Note extends Severity {
      def codeString(implicit printConfig : Printer.Config): String = "sim.Note"
    }
    case object Warning extends Severity {
      def codeString(implicit printConfig : Printer.Config): String = "sim.Warning"
    }
    case object Error extends Severity {
      def codeString(implicit printConfig : Printer.Config): String = "sim.Error"
    }
    case object Failure extends Severity {
      def codeString(implicit printConfig : Printer.Config): String = "sim.Failure"
    }
  }

  final case class Finish(ownerRef : DFBlock.Ref, tags : DFMember.Tags.Basic) extends DFSimMember {
    protected[ZFiant] def =~(that : DFMember)(implicit getSet : MemberGetSet) : Boolean = that match {
      case Finish(_, tags) => this.tags =~ tags
      case _ => false
    }
    def codeString(implicit printConfig : Printer.Config) : String = {
      import printConfig._
      s"$DF sim.$DF finish()"
    }
    def setTags(tagsFunc : DFMember.Tags.Basic => DFMember.Tags.Basic)(implicit getSet : MemberGetSet) : DFMember = getSet.set(this)(m => m.copy(tags = tagsFunc(m.tags)))
  }
  object Finish {
    def apply(to: DFAny, from: DFAny)(implicit ctx: DFAny.Context)
    : Finish = {
      implicit lazy val ret : Finish with DFMember.RefOwner =
        ctx.db.addMember(Finish(ctx.owner, ctx.meta)).asRefOwner
      ret
    }

  }
}
