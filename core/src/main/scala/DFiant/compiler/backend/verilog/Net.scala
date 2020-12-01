package DFiant
package compiler
package backend
package verilog
import compiler.printer.formatter._

private object Net {
  object External {
    def unapply(net : DFNet)(implicit printer: Printer) : Option[String] = {
        net match {
          case _ : DFNet if net.isConnection && net.hasLateConstruction =>
            val toValStr = Value.ref(net.toRef.get)
            val fromValStr = Value.ref(net.fromRef.get)
            if (net.toRef.isMemberOfDesign(net.getOwnerDesign)) Some(s".$toValStr${ALGN(0)}($fromValStr)")
            else Some(s".$fromValStr${ALGN(0)}($toValStr)")
          case _ => None
        }
    }
  }
  object Internal {
    def unapply(net : DFNet)(implicit printer: Printer) : Option[String] = {
      import printer.config._
      val toVal = net.toRef.get
      val fromVal = net.fromRef.get
      val toValStr = Value.ref(toVal)
      lazy val fromValStr = Value.ref(fromVal)
      net match {
        case DFNet.Inlined() => None
        case _ if net.hasLateConstruction => None
        case _ =>
          val op = if (net.toRef.get.isTaggedWith(RTL.Tag.Mod.Reg)) "<="
          else "="
          (toVal, fromVal) match {
            case (DFVector(_,_), DFAny.Const(_,DFVector.Token(_,cellTokens),_,_)) =>
              Some(cellTokens.zipWithIndex
                .map{case (v, i) => s"${toVal.name}[$i] ${ALGN(0)}$op ${Value.const(v)};"}.mkString("\n"))
            case (DFVector(_,cellNum), DFVector(_,_)) =>
              val iterName = "for_i"
              Some(s"""$KW for ($iterName = $LIT 0; $iterName < $LIT$cellNum; $iterName = $iterName + $LIT 1) $KW begin
                 |  $toValStr[$iterName] ${ALGN(0)}$op $fromValStr[$iterName];
                 |$KW end""".stripMargin)
            case _ =>
              Some(s"$toValStr ${ALGN(0)}$op $fromValStr;")
          }
      }
    }
  }
}
