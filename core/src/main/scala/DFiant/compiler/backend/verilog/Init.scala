package DFiant
package compiler.backend.verilog
import printer.formatter._

private object Init {
  def apply(member : DFAny)(implicit printer : Printer) : String = {
    import printer.config._
    member.getInit match {
      case Some(token +: Nil) if !token.isBubble && !member.isPortIn =>
        s"\n$KW initial ${ALGN(0)}${member.name} = ${Value.const(token)};"
      case _ => ""
    }
  }
}
