package DFiant
package compiler
package backend
package vhdl
import compiler.printer.formatter._

private object Net {
  object External {
    def unapply(net : DFNet)(implicit printer: Printer) : Option[String] = {
        import printer.config._
        val toVal = Value.ref(net.toRef.get)
        val fromVal = Value.ref(net.fromRef.get)
        net match {
          case _ : DFNet if net.isConnection && net.hasLateConstruction =>
            if (net.toRef.isMemberOfDesign(net.getOwnerDesign)) Some(s"$toVal ${ALGN(0)}=> $fromVal")
            else Some(s"$fromVal ${ALGN(0)}=> $toVal")
          case _ => None
        }
    }
  }
  object Internal {
    def unapply(net : DFNet)(implicit printer: Printer) : Option[String] = {
      import printer.config._
      val toVal = Value.ref(net.toRef.get)
      val fromVal = Value.ref(net.fromRef.get)
      net match {
        case DFNet.Inlined() => None
        case _ if net.hasLateConstruction => None
        case DFNet.Connection(_,_,_,_) | RTL.Net() | DFNet.Assignment(DFAny.In() | DFAny.Out(),_,_,_) =>
          Some(s"$toVal ${ALGN(0)}<= $fromVal;")
        case _ if printer.getSet.designDB.getAssignmentsFrom(net.toRef.get).exists(x => x.isTaggedWith(RTL.Tag.Mod.Reg)) =>
          Some(s"$toVal ${ALGN(0)}<= $fromVal;")
        case _ if net.isAssignment =>
          Some(s"$toVal ${ALGN(0)}:= $fromVal;")
        case _ => None
      }
    }
  }
}
