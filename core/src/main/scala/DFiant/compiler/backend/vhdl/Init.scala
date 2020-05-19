package DFiant
package compiler.backend.vhdl

private object Init {
  def apply(member : DFAny)(implicit printer : Printer, revision: VHDLRevision) : String = member.tags.init match {
    case Some(token +: Nil) if !token.isBubble => s" := ${Value.const(token)}"
    case _ => ""
  }
}
