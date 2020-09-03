package DFiant
package compiler.backend.vhdl

private object Init {
  def apply(member : DFAny.Member)(implicit printer : Printer) : String = member match {
    case dcl : DFAny.Dcl => dcl.externalInit match {
      case Some(token +: Nil) if !token.isBubble => s" := ${Value.const(token)}"
      case _ => ""
    }
    case _ => ""
  }
}
