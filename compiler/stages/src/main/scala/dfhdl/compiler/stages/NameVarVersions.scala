package dfhdl.compiler.stages

import dfhdl.compiler.analysis.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.patching.*
import dfhdl.options.CompilerOptions
import dfhdl.internals.*
import scala.collection.mutable
import dfhdl.core.DomainType.RT

//format: off
/** This stage materializes POSITIONAL reads of sequential-sink statements under RT domain bodies
  * into explicit version variables.
  *
  * An RT domain body is a sequential program executed once per clock step, so a wire read
  * observes the latest prior assignment within the step. `ToED` moves sequential-sink statements
  * (register and shared-variable writes; text outputs) into the clocked process, where every
  * read observes the SETTLED value of the combinational network instead. The two disagree
  * exactly when the read wire is reassigned after the statement's position. This stage closes
  * that gap ahead of `ToED`: each such read is captured into a fresh version variable at the
  * statement's position, so all sequential-sink reads become settled and `ToED` may move the
  * statements as-is.
  *
  * This is the `NameRegAliases` versioning mechanism at the process boundary (there, a `.reg` on
  * a multi-assigned wire snapshots the wire's positional value into `x_verN_reg`); here the
  * snapshot stays combinational, since it crosses only from `process(all)` to the clocked
  * process within the same step. Naming follows the same convention without the `_reg` suffix:
  * a single capture of `x` yields `x_ver`, several yield `x_ver1`, `x_ver2`, ...
  *
  * ==Rule 1: Unsettled sequential-sink reads are captured at the statement's position==
  *
  * A read wire is UNSETTLED at a statement when it is assigned again later in the body. The
  * version variable takes a don't-care default at the declarations area (its value is consumed
  * only when the capturing statement's guard path fired) and captures the wire right before the
  * statement's read cone, inside the same guard context.
  * {{{
  * // Before
  * v := x
  * if (we) ram(addr) := v
  * v := v | h"0f"
  * y := v
  *
  * // After
  * val v_ver = Bits(8) <> VAR
  * v_ver := ?
  * v := x
  * if (we)
  *   v_ver := v
  *   ram(addr) := v_ver
  * v := v | h"0f"
  * y := v
  * }}}
  *
  * ==Rule 2: Settled reads are left alone==
  *
  * Reads of inputs, constants, other-domain signals, named immutable values, register outputs,
  * and shared variables (whose writes commit at the step's end) are position-independent, and so
  * is a wire whose last assignment precedes the statement. The common case therefore needs no
  * captures at all.
  *
  * Sequential sinks are: shared-variable writes, register (`.din`) writes for registers whose
  * `.din` is never read (a din-read register lowers through the din-shadow form, whose
  * combinational assignments keep positional semantics natively), and text outputs (they fire
  * once per step inside the clocked process). Statements inside processes are untouched, and
  * statements inside loops are skipped in v1 (loops are atomic for `ToED` slicing). A site whose
  * GUARD path reads an unsettled wire is also skipped in v1 (`ToED` falls back to the shadow
  * form for it).
  */
//format: on
case object NameVarVersions extends HierarchyStage:
  // the input must be the final RT body shape: DF domains converted (ToRT), RT processes
  // lowered (DropRTProcess), `.reg` aliases made explicit din writes (NameRegAliases), and
  // conditional-expression assignments split into per-branch assignments
  // (ExplicitCondExprAssign; before it, a branch read hides behind the conditional header and
  // the read-cone walk cannot reach it). SimpleOrderMembers guarantees declarations-first
  // ordering, which the declarations-area anchor (and the default-before-any-capture
  // invariant) relies on.
  def dependencies: List[Stage] = List(
    ToRT, DropRTProcess, NameRegAliases, ExplicitNamedVars, ExplicitCondExprAssign,
    SimpleOrderMembers
  )
  def nullifies: Set[Stage] = Set(DFHDLUniqueNames)

  def transformSubDB(rootDB: DB)(using
      getSet: MemberGetSet,
      co: CompilerOptions,
      rg: RefGen
  ): DB =
    val results = subDB.namedOwnerMemberList.flatMap {
      case (domainOwner: (DFDomainOwner & DFBlock), members) =>
        domainOwner.domainType match
          case DomainType.RT => Some(domainPatches(domainOwner, members))
          case _             => None
      case _ => None
    }
    // Two-phase application (a genuinely unmergeable combination): phase 1 adds the version
    // declarations, defaults, and capture assignments; the redirect phases then re-reference
    // each capture's read set through `Replace(ChangeRefOnly)` patches keyed on the read leaf.
    // A leaf captured by several sites needs one redirect WAVE per site (two `Replace` patches
    // on the same leaf never merge), so wave k carries the k-th capture of every leaf.
    val allAdds = results.flatMap(_._1)
    val maxWaves = results.map(_._2.size).maxOption.getOrElse(0)
    val allWaves = (0 until maxWaves).toList.map(k => results.flatMap(_._2.lift(k).getOrElse(Nil)))
    allWaves.foldLeft(subDB.patch(allAdds))((db, wave) => db.patch(wave))
  end transformSubDB

  // one capture: `leaf` read positionally by the sequential-sink `site`; every ref to `leaf`
  // from `redirectSet` moves to the version variable, and the capture assignment is planted
  // right before `anchor` (the earliest redirected member, so the whole read cone follows it)
  private final case class SiteCapture(
      leaf: DFVal.Dcl,
      site: DFMember,
      redirectSet: List[DFMember],
      anchor: DFMember
  )

  // returns (phase-1 Add patches, redirect waves: waves(k) holds the k-th capture of each leaf)
  private def domainPatches(
      domainOwner: DFDomainOwner & DFBlock,
      members: List[DFMember]
  )(using MemberGetSet, RefGen): (List[(DFMember, Patch)], List[List[(DFMember, Patch)]]) =
    val analysis = new RTDomainAnalysis(domainOwner, members)
    import analysis.{
      posOf, lastAssignPos, dinReadREGs, readConeAndLeaves, isInLoop,
      guardPathHazard
    }
    if (lastAssignPos.isEmpty) (Nil, Nil) // no wires assigned, so nothing can be unsettled
    else
      // v1: a site whose guard path reads an unsettled wire is skipped entirely (see
      // `RTDomainAnalysis.guardPathHazard`), and `ToED` falls back to the shadow form for it
      val captures = mutable.ListBuffer.empty[SiteCapture]
      members.foreach { m =>
        // Some(excludeDcl) when `m` is a sequential-sink statement
        val seqSink: Option[Option[DFVal.Dcl]] = m match
          case DFNet.Assignment(toVal, _) =>
            toVal.departialDcl.collect {
              case (dcl, _)
                  if dcl.modifier.isShared || (dcl.isReg && !dinReadREGs.contains(dcl)) =>
                Some(dcl)
            }
          case _: TextOut => Some(None)
          case _          => None
        seqSink match
          case Some(excludeDcl) if !m.isInProcess && !isInLoop(m) && !guardPathHazard(m) =>
            val (cone, leaves) = readConeAndLeaves(m, excludeDcl)
            val sitePos = posOf(m)
            val hazardous = leaves.toList
              .filter(l => lastAssignPos.get(l).exists(_ > sitePos))
              .sortBy(l => (posOf.getOrElse(l, -1), l.getName))
            hazardous.foreach { leaf =>
              val redirectSet = (cone + m).toList.sortBy(posOf)
              captures += SiteCapture(leaf, m, redirectSet, redirectSet.minBy(posOf))
            }
          case _ =>
      }
      if (captures.isEmpty) (Nil, Nil)
      else
        val addPatches = mutable.ListBuffer.empty[(DFMember, Patch)]
        // version declarations (and their don't-care defaults) go to the declarations area,
        // mirroring `NameRegAliases`' placement
        val (posMember, addCfg) = members.view.reverse.dropWhile {
          case dcl: DFVal.Dcl if dcl.getOwnerDomain == domainOwner => false
          case _                                                   => true
        }.headOption match
          case Some(lastDcl) => (lastDcl, Patch.Add.Config.After)
          case None          => (domainOwner, Patch.Add.Config.InsideFirst)
        val verOf = mutable.Map.empty[SiteCapture, DFVal]
        val byLeaf = captures.toList.groupByOrdered(_.leaf)
        val declDsn = new MetaDesign(posMember, addCfg, domainType = RT):
          byLeaf.foreach { (leaf, leafCaps) =>
            leafCaps.zipWithIndex.foreach { (cap, i) =>
              val verName =
                if (leafCaps.sizeIs == 1) s"${leaf.getName}_ver"
                else s"${leaf.getName}_ver${(i + 1).toPaddedString(leafCaps.size)}"
              val verVar = leaf.asValAny.genNewVar(using dfc.setMeta(leaf.meta.setName(verName)))
              // don't-care default: the version is consumed only when the capturing site's
              // guard path fired, and the default prevents latch inference in `process(all)`
              verVar.asVarAny := dfhdl.core.Bubble.constValOf(verVar.dfType, named = false)
              verOf += cap -> verVar.asIR
            }
          }
        addPatches += declDsn.patch
        captures.foreach { cap =>
          // the capture assignment precedes the site's whole read cone, inside its guard context
          val capDsn = new MetaDesign(cap.anchor, Patch.Add.Config.Before, domainType = RT):
            verOf(cap).asValAny.asVarAny.:=(cap.leaf.asValAny)(using
              dfc.setMetaAnon(cap.site.meta.position)
            )
          addPatches += capDsn.patch
        }
        val waves = (0 until byLeaf.map(_._2.size).max).toList.map { k =>
          byLeaf.flatMap { (leaf, leafCaps) =>
            leafCaps.lift(k).map { cap =>
              leaf -> Patch.Replace(
                verOf(cap),
                Patch.Replace.Config.ChangeRefOnly,
                Patch.Replace.RefFilter.OfMembers(cap.redirectSet.toSet)
              )
            }
          }
        }
        (addPatches.toList, waves)
      end if
    end if
  end domainPatches
end NameVarVersions

extension [T: HasDB](t: T)
  def nameVarVersions(using CompilerOptions): DB =
    StageRunner.run(NameVarVersions)(t.db)
