package DFiant
package compiler
package backend
package verilog
import printer.formatter._

private object Net {
  object External {
    def unapply(net : DFNet)(implicit printer: Printer) : Option[String] = {
        val toVal = Value.ref(net.toRef.get)
        val fromVal = Value.ref(net.fromRef.get)
        net match {
          case _ : DFNet.Connection if net.hasLateConstruction =>
            if (net.toRef.isMemberOfDesign(net.getOwnerDesign)) Some(s".$toVal${ALGN(0)}($fromVal)")
            else Some(s".$fromVal${ALGN(0)}($toVal)")
          case _ => None
        }
    }
  }
  object Internal {
    def unapply(net : DFNet)(implicit printer: Printer) : Option[String] = {
      val toVal = Value.ref(net.toRef.get)
      val fromVal = Value.ref(net.fromRef.get)
      net match {
        case DFNet.Inlined() => None
        case _ if net.hasLateConstruction => None
        case _ if net.toRef.get.isTaggedWith(RTL.Tag.Mod.Reg) =>
          Some(s"$toVal ${ALGN(0)}<= $fromVal;")
        case _ : DFNet =>
          Some(s"$toVal ${ALGN(0)}= $fromVal;")
        case _ => None
      }
    }
  }
}
