package DFiant
package compiler

import csprinter.CSPrinter
import printer.formatter._

import scala.annotation.tailrec

final class SanityCheck[D <: DFDesign](c: IRCompilation[D]) {
  private val designDB = c.db
  import designDB.__getset
  implicit val printer: CSPrinter = new CSPrinter {
    val getSet: MemberGetSet     = __getset
    val config: CSPrinter.Config = implicitly[CSPrinter.Config]
  }
  private def memberExistenceCheck(): Unit = {
    val violations = designDB.members.flatMap {
      case n @ DFNet.Assignment(toVal, fromVal, _, _) =>
        val toValMissing = !designDB.members.contains(toVal)
        val fromValMissing = fromVal match {
          case _: DFAny.Const => false
          case _              => !designDB.members.contains(fromVal)
        }
        if (toValMissing) {
          println(
            s"Foreign value ${toVal.name} at net ${n.codeString.unformatted}"
          )
          designDB.members.collectFirst {
            case m if m.name == toVal.name => m
          } match {
            case Some(value) => println(s"Found:\n$value\nInstead of:\n$toVal")
            case None        =>
          }
        }
        if (fromValMissing) {
          println(
            s"Foreign value ${fromVal.name} at net ${n.codeString.unformatted}"
          )
          designDB.members.collectFirst {
            case m if m.name == fromVal.name => m
          } match {
            case Some(value) =>
              println(s"Found:\n$value\nInstead of:\n$fromVal")
            case None =>
          }
        }
        if (toValMissing || fromValMissing) Some(n)
        else None
      case _ => None
    }
    require(violations.isEmpty, "Failed member existence check!")
  }
  @tailrec private def ownershipCheck(
      currentOwner: DFOwner,
      members: List[DFMember]
  ): Unit = {
    members match {
      case m :: nextMembers if (m.getOwner == currentOwner) =>
        m match { //still in current owner
          case o: DFOwner => ownershipCheck(o, nextMembers) //entering new owner
          case _ =>
            ownershipCheck(currentOwner, nextMembers) //new non-member found
        }
      case (_: DFAny.Const) :: nextMembers => //we do not care about constant ownership
        ownershipCheck(currentOwner, nextMembers)
      case Nil => //Done! All is OK
      case m :: _ => //not in current owner
        if (currentOwner.isTop) {
          println(
            s"The member ${m.hashCode().toHexString}:\n$m\nHas owner ${m.getOwner.hashCode().toHexString}:\n${m.getOwner}"
          )
          val idx        = designDB.members.indexOf(m)
          val prevMember = designDB.members(idx - 1)
          println(
            s"Previous member ${prevMember.hashCode().toHexString}:\n$prevMember\nHas owner ${prevMember.getOwner
              .hashCode()
              .toHexString}:\n${prevMember.getOwner}"
          )
          require(false, "Failed ownership check!")
        }
        ownershipCheck(currentOwner.getOwner, members) //exiting current owner
    }
  }
  def sanityCheck: IRCompilation[D] = {
    memberExistenceCheck()
    ownershipCheck(designDB.top, designDB.members.drop(1))
    c
  }
}
