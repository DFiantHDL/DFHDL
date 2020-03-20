package ZFiant
package compiler.backend.vhdl
import compiler.sync.Sync

private object Net {
  object External {
    def unapply(net : DFNet)(implicit printer: Printer) : Option[String] = {
        import printer.config._
        import formatter._
        val toVal = Value.ref(net.toRef.get)
        val fromVal = Value.ref(net.fromRef.get)
        net match {
          case _ : DFNet.Connection if net.hasLateConstruction =>
            if (net.toRef.isMemberOfDesign(net.getOwnerDesign)) Some(s"$toVal ${ALGN(0)}=> $fromVal")
            else Some(s"$fromVal ${ALGN(0)}=> $toVal")
          case _ => None
        }
    }
  }
  object Internal {
    def unapply(net : DFNet)(implicit printer: Printer) : Option[String] = {
      import printer.config._
      import formatter._
      val toVal = Value.ref(net.toRef.get)
      val fromVal = Value.ref(net.fromRef.get)
      net match {
        case DFNet.Inlined() => None
        case _ if net.hasLateConstruction => None
        case _ : DFNet.Connection | Sync.Net() =>
          Some(s"$toVal ${ALGN(0)}<= $fromVal;")
        case _ : DFNet.Assignment =>
          Some(s"$toVal ${ALGN(0)}:= $fromVal;")
        case _ => None
      }
    }
  }
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
