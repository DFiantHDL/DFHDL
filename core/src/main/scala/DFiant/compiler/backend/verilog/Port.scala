package DFiant
package compiler.backend.verilog

import compiler.printer.formatter._

object Port {
  def apply(name: String, dir: String, rtType: String, arrayDim: String = "")(
      implicit printer: Printer
  ): String = {
    import printer.config._
    s"$dir $rtType ${ALGN(0)}$name$arrayDim"
  }
  object Dir {
    object In {
      def apply()(implicit printer: Printer): String = {
        import printer.config._
        s"$KW input  $KW wire"
      }
    }
    object Out {
      def apply()(implicit printer: Printer): String = {
        import printer.config._
        s"$KW output $KW reg "
      }
    }
  }
}

object Reg {
  def apply(
      name: String,
      rtType: String,
      initStr: String,
      arrayDim: String = ""
  )(implicit
      printer: Printer
  ): String = {
    import printer.config._
    val dclStrNoInit =
      s"$KW reg         $rtType ${ALGN(0)}$name$arrayDim"
    if (initStr.contains("\n")) s"$dclStrNoInit;\n$initStr"
    else s"$dclStrNoInit$initStr;"
  }
}

//Wires are only used to connect hierarchies
object Wire {
  def apply(name: String, rtType: String, arrayDim: String = "")(implicit
      printer: Printer
  ): String = {
    import printer.config._
    s"$KW wire        $rtType ${ALGN(0)}$name$arrayDim;"
  }
}
