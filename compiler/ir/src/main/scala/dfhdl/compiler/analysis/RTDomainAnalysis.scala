package dfhdl.compiler.analysis

import dfhdl.compiler.ir.*
import scala.annotation.tailrec
import scala.collection.mutable

/** Positional-read (settledness) analysis over a single RT domain body.
  *
  * An RT domain body is a sequential program executed once per clock step, so a wire read observes
  * the latest prior assignment within the step. A domain-assigned wire is SETTLED at a position
  * when no assignment to it appears later in the body, in which case its positional value equals
  * its end-of-step (settled) value and the read may safely move across a process boundary.
  * Registers and shared variables are settled by construction: their writes commit at the step's
  * end.
  *
  * Shared by `NameVarVersions` (which captures unsettled reads into version variables ahead of
  * `ToED`), by `ToED` itself (which slices sequential-sink statements into the clocked process and
  * must prove their reads settled), and by [[DB.sharedVarCheck]] (which rejects shared-variable
  * writes that no capture can make movable).
  */
final class RTDomainAnalysis(
    domainOwner: DFDomainOwner & DFBlock,
    members: List[DFMember]
)(using MemberGetSet):
  val posOf: Map[DFMember, Int] = members.view.zipWithIndex.toMap
  // A DIN read yields the register's pending value AT THE POSITION OF THE READ, so the
  // statement holding it must stay in the process body, in order. Promoting it to a
  // concurrent connection would instead read the shadow variable's final value, and when
  // the same statement also feeds that shadow it closes a combinational loop:
  // {{{
  // always_comb begin r_din = r; r_din = sum; end
  // assign sum = r_din + 8'd1;   // sum depends on r_din depends on sum
  // }}}
  private def readsDIN(net: DFNet): Boolean = net.collectRelMembers.exists {
    case _: DFVal.Alias.RegDIN => true
    case _                     => false
  }
  // a shared variable is only accessible inside processes (`DB.sharedVarCheck`), so a
  // statement reading one must not be promoted to a concurrent connection either
  private def readsShared(net: DFNet): Boolean = (net :: net.collectRelMembers).exists {
    _.getRefs.view.filterNot(_.isTypeRef).exists(_.get match
      case dcl: DFVal.Dcl => dcl.modifier.isShared
      case _              => false)
  }
  // every non-process assignment per wire (non-REG, non-shared) declaration, in order
  private val wireAssignPos: Map[DFVal.Dcl, List[(DFNet, Int)]] =
    val acc = mutable.Map.empty[DFVal.Dcl, mutable.ListBuffer[(DFNet, Int)]]
    members.foreach {
      case net @ DFNet.Assignment(toVal, _) if !net.isInProcess =>
        toVal.departialDcl.foreach { (dcl, _) =>
          if (!dcl.isReg && !dcl.modifier.isShared)
            acc.getOrElseUpdate(dcl, mutable.ListBuffer.empty) += (net -> posOf(net))
        }
      case _ =>
    }
    acc.view.mapValues(_.toList).toMap
  // a wire's single whole-target domain-level assignment is promoted by `ToED` to a concurrent
  // connection, making the wire a settled combinational signal at every position
  val connectionWireNets: Set[DFNet] =
    wireAssignPos.view.collect {
      case (dcl, List((net, _)))
          if net.lhsRef.get.isInstanceOf[DFVal.Dcl] && net.getOwner == domainOwner &&
            !readsDIN(net) && !readsShared(net) =>
        net
    }.toSet
  // the last assignment position of every wire that stays positional; REG outputs, shared
  // variables (their writes commit at the step's end), and connection-promoted wires are
  // settled by construction
  val lastAssignPos: Map[DFVal.Dcl, Int] =
    wireAssignPos.view.collect {
      case (dcl, assigns) if !assigns.forall((net, _) => connectionWireNets.contains(net)) =>
        dcl -> assigns.map(_._2).max
    }.toMap
  // REGs whose `.din` is read; they lower through the din-shadow form, whose combinational
  // assignments keep positional semantics natively, so they are not sequential sinks
  val dinReadREGs: Set[DFVal.Dcl] = members.view.collect {
    case alias: DFVal.Alias.RegDIN => alias.relValRef.get.departialDcl.map(_._1)
  }.flatten.toSet

  def settledAt(leaf: DFVal.Dcl, pos: Int): Boolean =
    lastAssignPos.get(leaf).forall(_ <= pos)

  // walks the anonymous read cone of `m`, collecting the cone members and the named
  // declarations it bottoms at (`excludeDcl` is the write target: a sink, not a read)
  def readConeAndLeaves(
      m: DFMember,
      excludeDcl: Option[DFVal.Dcl]
  ): (Set[DFMember], Set[DFVal.Dcl]) =
    val cone = mutable.Set.empty[DFMember]
    val leaves = mutable.Set.empty[DFVal.Dcl]
    def walk(target: DFMember): Unit = target match
      case dcl: DFVal.Dcl => if (!excludeDcl.contains(dcl)) leaves += dcl
      case dfVal: DFVal if dfVal.isAnonymous && !cone.contains(dfVal) =>
        cone += dfVal
        dfVal.getRefs.view.filterNot(_.isTypeRef).foreach(r => walk(r.get))
      case _ => // a named value is settled at its own position; nothing else is a read
    m.getRefs.view.filterNot(_.isTypeRef).foreach(r => walk(r.get))
    (cone.toSet, leaves.toSet)

  def isInLoop(m: DFMember): Boolean = m.isOwnedCond(cond = {
    case _: DFLoop.Block  => Some(true)
    case _: DFDomainOwner => Some(false)
    case _                => None
  })

  // the topmost loop enclosing `m` within the domain body, if any
  def loopRootOf(m: DFMember): Option[DFLoop.Block] =
    @tailrec def recur(m: DFMember, found: Option[DFLoop.Block]): Option[DFLoop.Block] =
      m.getOwner match
        case owner if owner == domainOwner => found
        case loop: DFLoop.Block            => recur(loop, Some(loop))
        case owner: DFBlock                => recur(owner, found)
    recur(m, None)

  // true when the guard path of `site` reads an unsettled wire; all the chain guards up to the
  // site's block are evaluated to reach it, each at the chain header's position (branches
  // between the header and a guard are mutually exclusive with reaching that guard, so the
  // header position is the sound reference for all of them)
  def guardPathHazard(site: DFMember): Boolean =
    @tailrec def pathBlocks(
        m: DFMember,
        acc: List[DFConditional.Block]
    ): List[DFConditional.Block] =
      m.ownerRef.get match
        case cb: DFConditional.Block       => pathBlocks(cb, cb :: acc)
        case owner if owner == domainOwner => acc
        case owner: DFBlock                => pathBlocks(owner, acc)
        case _                             => acc
    pathBlocks(site, Nil).exists { pathBlock =>
      @tailrec def chainToHeader(
          cb: DFConditional.Block,
          acc: List[DFConditional.Block]
      ): (DFConditional.Header, List[DFConditional.Block]) =
        cb.prevBlockOrHeaderRef.get match
          case prev: DFConditional.Block    => chainToHeader(prev, cb :: acc)
          case header: DFConditional.Header => (header, cb :: acc)
      val (header, blocks) = chainToHeader(pathBlock, Nil)
      val headerPos = posOf(header)
      (header :: blocks).exists { m =>
        readConeAndLeaves(m, None)._2
          .exists(l => lastAssignPos.get(l).exists(_ > headerPos))
      }
    }
  end guardPathHazard

  // a statement (or guard) may move into the clocked process only when its whole read cone is
  // free of `.din` reads (positional by nature; their shadow variable lives in `process(all)`)
  // and bottoms at declarations settled at the given position
  def coneAndLeavesOK(m: DFMember, excludeDcl: Option[DFVal.Dcl], pos: Int): Boolean =
    val (cone, leaves) = readConeAndLeaves(m, excludeDcl)
    !cone.exists { case _: DFVal.Alias.RegDIN => true; case _ => false } &&
    leaves.forall(settledAt(_, pos))
  def stmtMovable(m: DFMember, excludeDcl: Option[DFVal.Dcl]): Boolean =
    coneAndLeavesOK(m, excludeDcl, posOf(m)) && !guardPathHazard(m)
  // no position capture can fix these: a guard-path hazard (v1 skips guard captures) or a
  // `.din` read (its shadow variable only exists in `process(all)`)
  def stmtUncapturable(m: DFMember, excludeDcl: Option[DFVal.Dcl]): Boolean =
    guardPathHazard(m) ||
      readConeAndLeaves(m, excludeDcl)._1.exists {
        case _: DFVal.Alias.RegDIN => true
        case _                     => false
      }

  // loops are atomic for `ToED` slicing: a loop moves whole into the clocked process when all
  // its content is sequential-sink writes to directly-moving targets (shared variables, and
  // REGs outside `shadowREGs`) and every read (guards and ranges included) is settled at the
  // loop's position; otherwise it stays combinational whole
  def loopSeqMovable(loop: DFLoop.Block, shadowREGs: collection.Set[DFVal.Dcl]): Boolean =
    val loopPos = posOf(loop)
    val inner = loop :: loop.members(MemberView.Flattened)
    // for-loop ranges are owned outside the loop block, so collect them explicitly
    val ranges = inner.collect { case f: DFLoop.DFForBlock => f.rangeRef.get }
    (inner ++ ranges).forall {
      case net @ DFNet.Assignment(toVal, _) =>
        toVal.departialDcl.exists { (dcl, _) =>
          (dcl.modifier.isShared || (dcl.isReg && !shadowREGs.contains(dcl))) &&
          coneAndLeavesOK(net, Some(dcl), loopPos)
        }
      case _: DFNet => false
      case m        => coneAndLeavesOK(m, None, loopPos)
    } && !guardPathHazard(loop)
  end loopSeqMovable
end RTDomainAnalysis
