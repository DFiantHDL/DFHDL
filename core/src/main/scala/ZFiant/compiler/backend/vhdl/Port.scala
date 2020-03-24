package ZFiant
package compiler.backend.vhdl

object Port {
  def apply(name : String, dir : String, rtType : String, initStr : String)(
    implicit printer: Printer
  ) : String = {
    import printer.config._
    import formatter._

    s"$name ${ALGN(0)}: $dir $rtType$initStr"
  }
  object Dir {
    object In {
      def apply()(implicit printer : Printer) : String = s"${printer.config.KW}in "
    }
    object Out {
      def apply()(implicit printer : Printer) : String = s"${printer.config.KW}out"
    }
  }
}

object Signal {
  def apply(name : String, rtType : String, initStr : String)(
    implicit printer: Printer
  ) : String = {
    import printer.config._
    import formatter._

    s"$KW signal $name ${ALGN(0)}: $rtType$initStr;"
  }
}

object Variable {
  def apply(name : String, rtType : String, initStr : String)(
    implicit printer: Printer
  ) : String = {
    import printer.config._
    import formatter._

    s"$KW variable $name ${ALGN(0)}: $rtType$initStr;"
  }
}