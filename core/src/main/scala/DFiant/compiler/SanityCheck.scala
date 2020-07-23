package DFiant
package compiler

import DFiant.csprinter.CSPrinter
import printer.formatter._

final class SanityCheckOps[D <: DFDesign](c : IRCompilation[D]) {
  private val designDB = c.db
  import designDB.__getset
  def sanityCheck = {
    implicit val printer : CSPrinter = new CSPrinter {
      val getSet : MemberGetSet = __getset
      val config : CSPrinter.Config = implicitly[CSPrinter.Config]
    }
    val violations = designDB.members.flatMap {
      case _ : DFAny.Const => None
      case n @ DFNet.Assignment.Unref(toVal, fromVal, _,_) =>
        if (!designDB.members.contains(toVal)) {
          println(s"Foreign value ${toVal.name} at net ${n.codeString.unformatted}")
        }
        if (!designDB.members.contains(fromVal)) {
          println(s"Foreign value ${fromVal.name} at net ${n.codeString.unformatted}")
        }
        Some(n)
      case _ => None
    }
    require(violations.isEmpty)
    c
  }
}