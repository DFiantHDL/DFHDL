package DFiant
package compiler.backend.verilog
import printer.formatter._

private object Init {
  def apply(member : DFAny)(implicit printer : Printer) : String = {
    import printer.config._
    member.getInit match {
      case Some(token +: Nil) if !token.isBubble && !member.isPortIn =>
        s" = ${Value.const(token)}"
      case _ => ""
    }
  }
}
