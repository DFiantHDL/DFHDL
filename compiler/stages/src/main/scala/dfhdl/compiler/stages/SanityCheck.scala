package dfhdl.compiler.stages

import dfhdl.compiler.analysis.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.patching.*
import dfhdl.compiler.printing.*
import dfhdl.internals.*
import scala.annotation.tailrec

case object SanityCheck extends Stage:
  def dependencies: List[Stage] = List()
  def nullifies: Set[Stage] = Set()
  def refCheck()(using MemberGetSet): Unit =
    val refTable = getSet.designDB.refTable
    var hasViolations: Boolean = false
    getSet.designDB.members.foreach { m =>
      if (
        m.getRefs.exists {
          case _: DFRef.Empty => false
          case r              => !refTable.contains(r)
        }
      )
        hasViolations = true
        println(s"Missing ref for the member: $m")
      m match
        case m: DFDesignBlock if !m.isTop =>
          if (!refTable.contains(m.ownerRef))
            hasViolations = true
            println(s"Missing owner ref for the member: $m")
        case _ =>
    }
    require(!hasViolations, "Failed reference check!")
  end refCheck
  private def memberExistenceCheck()(using MemberGetSet): Unit =
    given Printer = DefaultPrinter
    val members = getSet.designDB.members
    val violations = members.flatMap {
      case n @ DFNet(toRef, DFNet.Op.Assignment, fromRef, _, _, _) =>
        val toMember = toRef.get
        val fromMember = fromRef.get
        val toValMissing = !members.contains(toMember)
        val fromValMissing = fromMember match
          case _: DFVal.Const => false
          case _              => !members.contains(fromMember)
        if (toValMissing)
          println(s"Foreign value ${toMember.name} at net ${n.codeString}")
          members.collectFirst {
            case m: DFMember.Named if m.name == toMember.name => m
          } match
            case Some(value) => println(s"Found:\n$value\nInstead of:\n$toMember")
            case None        =>
        if (fromValMissing)
          println(s"Foreign value ${fromMember.name} at net ${n.codeString}")
          members.collectFirst {
            case m: DFMember.Named if m.name == fromMember.name => m
          } match
            case Some(value) => println(s"Found:\n$value\nInstead of:\n$fromMember")
            case None        =>
        if (toValMissing || fromValMissing) Some(n)
        else None
      case _ => None
    }
    require(violations.isEmpty, "Failed member existence check!")
  end memberExistenceCheck
  @tailrec private def ownershipCheck(currentOwner: DFOwner, members: List[DFMember])(using
      MemberGetSet
  ): Unit =
    members match
      case m :: nextMembers if (m.getOwner == currentOwner) =>
        m match // still in current owner
          case o: DFOwner => ownershipCheck(o, nextMembers) // entering new owner
          case _          => ownershipCheck(currentOwner, nextMembers) // new non-member found
      case (_: DFVal.Const) :: nextMembers => // we do not care about constant ownership
        ownershipCheck(currentOwner, nextMembers)
      case Nil => // Done! All is OK
      case m :: _ => // not in current owner
        if (currentOwner.isTop)
          println(
            s"The member ${m.hashCode().toHexString}:\n$m\nHas owner ${m.getOwner.hashCode().toHexString}:\n${m.getOwner}"
          )
          val idx = getSet.designDB.members.indexOf(m)
          val prevMember = getSet.designDB.members(idx - 1)
          println(
            s"Previous member ${prevMember.hashCode().toHexString}:\n$prevMember\nHas owner ${prevMember.getOwner
                .hashCode()
                .toHexString}:\n${prevMember.getOwner}"
          )
          require(false, "Failed ownership check!")
        ownershipCheck(currentOwner.getOwner, members) // exiting current owner

  def transform(designDB: DB)(using MemberGetSet): DB =
    refCheck()
    memberExistenceCheck()
    designDB.check()
    ownershipCheck(designDB.top, designDB.members.drop(1))
    designDB
end SanityCheck

extension [T: HasDB](t: T) def sanityCheck: DB = StageRunner.run(SanityCheck)(t.db)
