package DFiant
package compiler.backend.vhdl

private object Init {
  def apply(member : DFAny.Member)(implicit printer : Printer) : String = {
    val tokenOption = member match {
      case dcl : DFAny.Dcl => dcl.externalInit match {
        case Some(token +: Nil) if !token.isBubble => Some(token)
        case _ => None
      }
      case const : DFAny.Const => Some(const.token)
      case _ => None
    }
    tokenOption match {
      case Some(token) => s" := ${Value.const(token)}"
      case None => ""
    }
  }
}
