package DFiant
package compiler.backend.verilog
import compiler.printer.formatter._

private object Init {
  def apply(member : DFAny)(implicit printer : Printer) : String = {
    import printer.config._
    member match {
      case dcl : DFAny.Dcl => dcl.externalInit match {
        case Some(token +: Nil) if !token.isBubble && !member.isPortIn =>
          s" = ${Value.const(token)}"
        case _ => ""
      }
      case _ => ""
    }
  }
}
