package ZFiant
package compiler.backend.vhdl

private object Net {
  object Assignment {
    def apply(toVal : String, fromVal : String)(implicit printer: Printer) : String = {
      import printer.config._
      import formatter._
      s"$toVal ${ALGN(0)}:= $fromVal;"
    }
  }
  object Connection {
    def apply(toVal : String, fromVal : String)(implicit printer : Printer) : String = {
      import printer.config._
      import formatter._
      s"$toVal ${ALGN(0)}<= $fromVal;"
    }
  }
}
