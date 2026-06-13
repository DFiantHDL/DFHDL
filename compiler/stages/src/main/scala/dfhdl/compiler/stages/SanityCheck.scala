package dfhdl.compiler.stages

import dfhdl.compiler.analysis.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.patching.*
import dfhdl.compiler.printing.*
import dfhdl.internals.*
import dfhdl.options.CompilerOptions
import scala.annotation.tailrec
import scala.collection.mutable

case class SanityCheck(skipAnonRefCheck: Boolean) extends Stage:
  def dependencies: List[Stage] = List()
  def nullifies: Set[Stage] = Set()
  def refCheck()(using MemberGetSet): Unit =
    val refTable = getSet.designDB.refTable
    val memberTable = getSet.designDB.memberTable
    var hasViolations: Boolean = false
    def reportViolation(msg: String): Unit =
      hasViolations = true
      System.err.println(msg)

    // for quick lookup of by-name port selections during ref checks
    val instPortsByNameSet = getSet.designDB.members.view.flatMap {
      case inst: DFDesignInst =>
        val design = inst.getDesignBlock
        design.members(MemberView.Folded).view.collect {
          case port: DFVal.Dcl if port.isPort => (inst, port.getRelativeName(design))
        }
      case _ => Nil
    }.toSet

    val memberSet = mutable.Set.empty[DFMember]
    // checks for all members
    getSet.designDB.members.foreach { m =>
      if (memberSet.contains(m))
        reportViolation(s"More than one appearance of member in member list: $m")
      else memberSet += m
      // check for missing references
      m.getRefs.foreach {
        case _: DFRef.Empty             => // do nothing
        case r if !refTable.contains(r) =>
          reportViolation(s"Missing ref $r for the member: $m")
        case _ => // do nothing
      }
      // check for circular references
      if (m.originMembers.exists(_ == m))
        reportViolation(s"Circular reference for the member: $m")
      m match
        // check for missing owner references
        case m: DFDesignBlock if !m.isTop =>
          if (!refTable.contains(m.ownerRef))
            reportViolation(s"Missing owner ref for the member: $m")
        // check that all blocks in a conditional chain share the same owner
        case cb: DFConditional.Block =>
          cb.prevBlockOrHeaderRef.get match
            case prevBlock: DFConditional.Block if prevBlock.getOwner != cb.getOwner =>
              reportViolation(
                s"""|Conditional block chain has mismatched owners.
                    |Block:      $cb
                    |Owner:      ${cb.getOwner}
                    |Prev block: $prevBlock
                    |Prev owner: ${prevBlock.getOwner}""".stripMargin
              )
            case _ =>
        // check by-name selectors
        case pbns: DFVal.PortByNameSelect =>
          val designInst = pbns.designInstRef.get
          // check port existence
          if (!instPortsByNameSet.contains((designInst, pbns.portNamePath)))
            reportViolation(
              s"""|By-name port selection references non-existent port.
                  |Design instance: ${designInst}
                  |Port name path:  ${pbns.portNamePath}""".stripMargin
            )
          // check usage
          if (pbns.originMembers.isEmpty)
            reportViolation(s"No references to the by-name port selection: ${pbns}")
        // check references always refer to internal design instance ports via
        // by name selections and never directly
        case m =>
          m.getRefs.foreach { r =>
            r.get match
              case port @ DclPort() =>
                if (!m.isSameOwnerDesignAs(port))
                  reportViolation(
                    s"""|Direct internal port referencing instead of by-name selection found.
                        |Referencing member: $m
                        |Referenced port: $port""".stripMargin
                  )
              case _ =>
          }
      end match
      // check that anonymous values are referenced exactly once
      m match
        case dfVal: DFVal if dfVal.isAllowedMultipleReferences => // skip named
        case dfVal: DFVal                                      =>
          val deps = dfVal.getReadDeps
          if (deps.size > 1)
            reportViolation(
              s"""|An anonymous value has more than one reference.
                  |Referenced value: $dfVal
                  |Referencing members: ${deps.mkString("\n\t", "\n\t", "")}""".stripMargin
            )
          if (!skipAnonRefCheck)
            if (dfVal.originMembers.isEmpty)
              dfVal match
                case ch: DFConditional.Header if ch.dfType == DFUnit =>
                case Ident(_)                                        =>
                case _                                               =>
                  reportViolation(
                    s"""|An anonymous value has no references.
                        |Referenced value: $dfVal""".stripMargin
                  )
        case range: DFRange if !skipAnonRefCheck && range.originMembers.isEmpty =>
          reportViolation(
            s"""|An anonymous range has no references.
                |Referenced range: $range""".stripMargin
          )
        case _ =>
      end match
    }
    val originRefTable = getSet.designDB.originRefTable
    // Collect every member's OneWay.Gen ownerRef so we can detect orphans —
    // a OneWay ref in refTable whose source member was removed but the ref
    // entry was left behind. Under the flat model these orphans are invisible
    // because a later Patch.Replace on the target rewires them via
    // `memberTable.invert`; under per-sub-DB patching a peer sub-DB's patch
    // can't reach them, so they surface as "Ref exists for a removed member"
    // after reassembly.
    val memberOwnerRefs = mutable.Set.empty[DFRefAny]
    memberSet.foreach { m =>
      memberOwnerRefs += m.ownerRef
      m match
        // DFDesignInst emits a OneWay ref to its DFDesignBlock via designRef,
        // outside of getRefs (which only carries TwoWay refs). Register it so
        // the orphan OneWay.Gen detection below does not flag it.
        case inst: DFDesignInst => memberOwnerRefs += inst.designRef
        case _                  =>
    }
    // checks for all references
    refTable.foreach { (r, m) =>
      if (!m.isInstanceOf[DFMember.Empty] && !memberSet.contains(m))
        reportViolation(s"Ref $r exists for a removed member: $m")
      r match
        case r: DFRef.TwoWayAny if !originRefTable.contains(r) =>
          reportViolation(s"Missing origin member with the reference $r for the member: $m")
        // Orphan-ref detection is gated on `!skipAnonRefCheck` because some
        // orphans are produced in elaboration / meta-design scaffolding and
        // only get swept up once `DropUnreferencedAnons` has run in the
        // pipeline. StageRunner invokes sanityCheck with skipAnonRefCheck=true
        // until DropUnreferencedAnons is in the `done` set, matching how the
        // anonymous-value check is already gated.
        case _: DFRef.OneWay.Gen[?]
            if !skipAnonRefCheck && !memberOwnerRefs.contains(r) =>
          reportViolation(s"Orphan OneWay ref $r has no source member (target: $m)")
        case _ => // do nothing
    }
    // check a reference is only used by a single member
    val originRefTableMutable = mutable.Map.empty[DFRefAny, DFMember]
    memberSet.foreach { m =>
      m.getRefs.foreach {
        case _: DFRef.Empty => // skip empty referenced
        case r              =>
          originRefTableMutable.get(r).foreach { prevMember =>
            def originViolation(addedText: String) = reportViolation(
              s"""|Ref $r has more than one origin member$addedText.
                  |Target member:   ${r.get}
                  |Origin member 1: $prevMember
                  |Origin member 2: $m""".stripMargin
            )
            r match
              case _: DFRef.TypeRef =>
                r.get match
                  // global references can be shared across types
                  case dfVal: DFVal.CanBeGlobal if dfVal.isGlobal => // no violation
                  case _                                          =>
                    if (!(prevMember isSameOwnerDesignAs m))
                      originViolation(" from a different design")
              case _ => originViolation("")
          }
          originRefTableMutable += r -> m
      }
    }
    // check a global member reference is anonymous only if the referencing member is global
    // or the referencing member is a design parameter
    originRefTableMutable.foreach {
      case (_: DFRef.TypeRef, _) => // do nothing
      case (r, originMember)     =>
        r.get match
          case targetVal: DFVal if targetVal.isAnonymous && targetVal.isGlobal =>
            originMember match
              case originVal: DFVal if originVal.isGlobal =>
              case _: DFVal.DesignParam                   =>
              case _: DFDesignInst => // paramMap entries may reference global anon vals
              case _               =>
                reportViolation(
                  s"""|A global anonymous member is referenced by a non-global member.
                      |Target member: ${targetVal}
                      |Origin member: ${originMember}""".stripMargin
                )
          case _ =>
    }
    require(!hasViolations, "Failed reference check!")
  end refCheck
  private def memberExistenceCheck()(using MemberGetSet): Unit =
    given Printer = DefaultPrinter
    val members = getSet.designDB.members
    val memberTable = getSet.designDB.memberTable
    val violations = members.flatMap {
      case n @ DFNet(lhsRef = toRef, op = DFNet.Op.Assignment, rhsRef = fromRef) =>
        val toMember = toRef.get
        val fromMember = fromRef.get
        val toValMissing = !memberTable.contains(toMember)
        val fromValMissing = fromMember match
          case _: DFVal.Const => false
          case _              => !memberTable.contains(fromMember)
        if (toValMissing)
          System.err.println(s"Foreign value ${toMember.getName} at net ${n.codeString}")
          members.collectFirst {
            case m: DFMember.Named if m.getName == toMember.getName => m
          } match
            case Some(value) => println(s"Found:\n$value\nInstead of:\n$toMember")
            case None        =>
        if (fromValMissing)
          System.err.println(s"Foreign value ${fromMember.getName} at net ${n.codeString}")
          members.collectFirst {
            case m: DFMember.Named if m.getName == fromMember.getName => m
          } match
            case Some(value) => println(s"Found:\n$value\nInstead of:\n$fromMember")
            case None        =>
        if (toValMissing || fromValMissing) Some(n)
        else None
      case _ => None
    }
    require(violations.isEmpty, "Failed member existence check!")
  end memberExistenceCheck
  // Walks the flat member list maintaining an explicit owner stack instead of
  // relying on getOwner to walk up — non-top DFDesignBlocks now resolve
  // `ownerRef` to DFMember.Empty under the new convention, so the parent must
  // be recovered from the walk position rather than the block itself. Treats
  // every DFDesignBlock encountered as entering a fresh scope; on mismatch
  // we pop until the next member's owner is on top of the stack.
  private def ownershipCheck(initialOwner: DFOwner, members: List[DFMember])(using
      MemberGetSet
  ): Unit =
    val ownerStack = mutable.Stack[DFOwner](initialOwner)
    var remaining: List[DFMember] = members
    while (remaining.nonEmpty)
      val m = remaining.head
      m match
        case d: DFDesignBlock =>
          // DFDesignBlock encountered: enter as a fresh scope.
          ownerStack.push(d)
          remaining = remaining.tail
        case _ if m.getOwner == ownerStack.top =>
          m match
            case o: DFOwner => ownerStack.push(o)
            case _          => // stay in current owner
          remaining = remaining.tail
        case _ =>
          // Member doesn't belong to current owner — pop until it does. If we
          // pop past the initial owner, ownership is genuinely violated.
          if (ownerStack.size == 1)
            println(
              s"The member ${m.hashString}:\n$m\nHas owner ${m.getOwner.hashString}:\n${m.getOwner}"
            )
            val idx = getSet.designDB.members.indexOf(m)
            val prevMember = getSet.designDB.members(idx - 1)
            println(
              s"Previous member ${prevMember.hashString}:\n$prevMember\nHas owner ${prevMember.getOwner
                  .hashCode()
                  .toHexString}:\n${prevMember.getOwner}"
            )
            require(false, "Failed ownership check!")
          ownerStack.pop()
      end match
    end while
  end ownershipCheck

  // checks that a member can only reference members that were defined before it
  private def orderCheck()(using MemberGetSet): Unit =
    val discoveredMembers = mutable.Set[DFMember](DFMember.Empty)
    var hasViolations: Boolean = false
    getSet.designDB.members.foreach {
      // goto statement can reference later steps
      case _: Goto =>
      case m       =>
        m.getRefs.foreach {
          case r @ DFRef(rm) if !discoveredMembers.contains(rm) =>
            m match
              case dfVal: DFVal if dfVal.isGlobal =>
                println(
                  s"The global member ${m.hashString}:\n$m\nHas reference $r pointing to a later member ${rm.hashString}:\n${rm}"
                )
              case _ =>
                val hierarchy =
                  if (m.ownerRef.get == DFMember.Empty) "<top>" else m.getOwnerNamed.getFullName
                println(
                  s"The member ${m.hashString}:\n$m\nIn hierarchy:\n$hierarchy\nHas reference $r pointing to a later member ${rm.hashString}:\n${rm}"
                )
            hasViolations = true
            require(!hasViolations, "Failed member order check!")
          case _ =>
        }
        discoveredMembers += m
    }
    require(!hasViolations, "Failed member order check!")
  end orderCheck

  // Temporary Phase 1 check: exercise the old<->new DB conversion round-trip
  // against every design the test suite exercises via sanityCheck. Under
  // B-pure, globals no longer live at root in the new-style DB and are
  // partitioned per sub-DB by closure; the elaboration order of globals is
  // not preserved across the round-trip. Compare globals by identity-set and
  // the rest of the members as an ordered list. RefTable, globalTags, and
  // srcFiles still must match exactly.
  private def hierarchicalDBRoundTripCheck(designDB: DB): Unit =
    val lhs = designDB.oldToNew.newToOld.canonicalForm
    val rhs = designDB.canonicalForm
    def partition(db: DB): (Set[DFMember], List[DFMember]) =
      given MemberGetSet = db.getSet
      val globals = mutable.Set.empty[DFMember]
      val nonGlobals = mutable.ListBuffer.empty[DFMember]
      db.members.foreach {
        case dfVal: DFVal.CanBeGlobal if dfVal.isGlobal => globals += dfVal
        case m                                          => nonGlobals += m
      }
      (globals.toSet, nonGlobals.toList)
    val (lhsGlobals, lhsNonGlobals) = partition(lhs)
    val (rhsGlobals, rhsNonGlobals) = partition(rhs)
    if (lhsGlobals != rhsGlobals)
      val onlyLhs = lhsGlobals -- rhsGlobals
      val onlyRhs = rhsGlobals -- lhsGlobals
      throw new IllegalArgumentException(
        s"""Hierarchical DB round-trip globals identity-set mismatch.
           |Only in lhs (round-trip): ${onlyLhs.mkString(", ")}
           |Only in rhs (input):     ${onlyRhs.mkString(", ")}""".stripMargin
      )
    require(
      lhsNonGlobals == rhsNonGlobals,
      "Hierarchical DB round-trip non-globals member list mismatch."
    )
    require(
      lhs.refTable == rhs.refTable,
      "Hierarchical DB round-trip refTable mismatch."
    )
    require(
      lhs.globalTags == rhs.globalTags && lhs.srcFiles == rhs.srcFiles,
      "Hierarchical DB round-trip globalTags or srcFiles mismatch."
    )
  end hierarchicalDBRoundTripCheck

  def transform(designDB: DB)(using MemberGetSet, CompilerOptions): DB =
    refCheck()
    memberExistenceCheck()
    ownershipCheck(designDB.top, designDB.membersNoGlobals.drop(1))
    orderCheck()
    designDB.oldToNew.check
    designDB.new_hierEquivalenceCheck()
    hierarchicalDBRoundTripCheck(designDB)
    designDB
end SanityCheck

extension [T: HasDB](t: T)
  def sanityCheck(skipAnonRefCheck: Boolean)(using CompilerOptions): DB =
    StageRunner.run(SanityCheck(skipAnonRefCheck))(t.db)
  def sanityCheck(using CompilerOptions): DB =
    t.sanityCheck(skipAnonRefCheck = false)
