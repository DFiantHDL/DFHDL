package dfhdl.compiler.stages

import dfhdl.compiler.analysis.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.patching.*
import dfhdl.internals.*
import dfhdl.options.CompilerOptions
import scala.collection.mutable
import dfhdl.core.DFOpaque as coreDFOpaque
import dfhdl.core.{asFE, DFCG}

/** Reads the resolved `@timing.clock` / `@timing.reset` / `@timing.related` annotations written by
  * [[ExplicitClkRstCfg]] onto each owner's meta, and:
  *
  *   - Adds clk/rst input ports to non-related RT domain owners that carry the corresponding
  *     annotations, unless already explicitly declared.
  *   - Memoizes the opaque clk/rst types by the annotation content tuples (so identical
  *     configurations share the same opaque type).
  *   - Generates the sim-driver block for top-level simulation designs.
  */
case object AddClkRst extends Stage:
  def dependencies: List[Stage] = List(ToRT, ExplicitClkRstCfg)
  def nullifies: Set[Stage] = Set(ViaConnection)
  def transform(designDB: DB)(using MemberGetSet, CompilerOptions): DB =
    given RefGen = RefGen.fromGetSet
    val defaultTag = designDB.globalTags.getTagOf[DefaultRTDomainCfgTag].get
    // saves (design, clkContent) pairs that are outputting clk, and
    // (design, clkGrpName, rstContent) triples for outputting rst. Rst is keyed together
    // with its paired clock's grpName so two rsts with identical mode/active but belonging
    // to different clock groups are treated as distinct.
    val designClkOut = mutable.Set.empty[(DFDesignBlock, constraints.Timing.Clock)]
    val designRstOut = mutable.Set.empty[(DFDesignBlock, String, constraints.Timing.Reset)]
    // memoize opaque types: clk by its content (grpName is a field — distinct grpNames
    // with otherwise-identical content yield distinct Clock instances and distinct keys).
    // rst is memoized by its paired clk's grpName + rst content, so rsts belonging to
    // different clock groups (even with the same mode/active) get distinct opaque types.
    val clkTypeMap = mutable.Map.empty[constraints.Timing.Clock, coreDFOpaque[coreDFOpaque.Clk]]
    val rstTypeMap =
      mutable.Map.empty[(String, constraints.Timing.Reset), coreDFOpaque[coreDFOpaque.Rst]]
    // saves opaque clk/rst type replacements
    val opaqueReplaceMap = mutable.Map.empty[DFOpaque, DFOpaque]

    // The opaque-type suffix comes from the clock's `grpName` (the Rst opaque uses the
    // same grpName as its paired clock — per design, reset doesn't have its own grpName).
    // After resolution the grpName is always populated (DefaultRTDomainCfgTag seeds
    // it with "Default"), so `.get` is safe.
    def grpName(clk: constraints.Timing.Clock): String = clk.grpName.get

    extension (domainOwner: DFDomainOwner)
      // Only IO constraints are moved from the owner onto the generated clk port —
      // `@timing.clock` / `@timing.reset` stay on the owner (the domain is the canonical
      // carrier of the timing configuration; the opaque Clk_<grp> / Rst_<grp> port type
      // encodes the same info into the clk/rst port's static type).
      def getClkConstraints: List[constraints.Constraint] =
        domainOwner.getConstraints.collect {
          case c: constraints.IO => c
        }
    end extension

    // Extract the resolved @timing.clock / @timing.reset from the owner's meta.
    // Ignores @timing.related (those owners don't get clk/rst ports here).
    def resolvedAnnots(owner: DFDomainOwner)
        : ClkRstTiming =
      val hasRelated = owner.meta.annotations.exists {
        case _: constraints.Timing.Related => true
        case _                             => false
      }
      if (hasRelated) (None, None)
      else
        val clk = owner.meta.annotations.collectFirst {
          case c: constraints.Timing.Clock => c
        }
        val rst = owner.meta.annotations.collectFirst {
          case r: constraints.Timing.Reset => r
        }
        (clk, rst)
    end resolvedAnnots

    val patchList: List[(DFMember, Patch)] = designDB.domainOwnerMemberList.flatMap {
      case (owner, members) =>
        val ownerClkConstraints = owner.getClkConstraints
        val design = owner.getThisOrOwnerDesign
        val ownerDomainPatchOption = owner.domainType match
          case DomainType.RT =>
            val (clkAnnotOpt, rstAnnotOpt) = resolvedAnnots(owner)
            // skip if no clk/rst slots (relaxed combinational or Related)
            if (clkAnnotOpt.isEmpty && rstAnnotOpt.isEmpty) None
            else
              // check for existing clk/rst dcls
              val existingClk = members.collectFirst {
                case clk: DFVal.Dcl if clk.isClkDcl =>
                  clkAnnotOpt.foreach { clkAnnot =>
                    if (clk.modifier.dir == DFVal.Modifier.OUT && !design.isTopTop)
                      designDB.designBlockOwnershipMap.getOrElse(design, Set.empty)
                        .foreach(parent => designClkOut += ((parent, clkAnnot)))
                  }
                  clk
              }
              val existingRst = members.collectFirst {
                case rst: DFVal.Dcl if rst.isRstDcl =>
                  (rstAnnotOpt, clkAnnotOpt) match
                    case (Some(rstAnnot), Some(clkAnnot))
                        if rst.modifier.dir == DFVal.Modifier.OUT && !design.isTopTop =>
                      designDB.designBlockOwnershipMap.getOrElse(design, Set.empty)
                        .foreach(parent => designRstOut += ((parent, grpName(clkAnnot), rstAnnot)))
                    case _ =>
                  rst
              }
              // "already handled by an internal design that has an OUT of the same config" check
              val clkAlreadyHandled = clkAnnotOpt.exists(c => designClkOut.contains((design, c)))
              val rstAlreadyHandled = (clkAnnotOpt, rstAnnotOpt) match
                case (Some(c), Some(r)) => designRstOut.contains((design, grpName(c), r))
                case _                  => false

              // required names from the resolved annotations
              val requiredClkName = clkAnnotOpt.collect {
                case c if !clkAlreadyHandled => c.portName.get
              }
              val requiredRstName = rstAnnotOpt.collect {
                case r if !rstAlreadyHandled => r.portName.get
              }
              // whether to add new clk/rst ports
              val addClk = requiredClkName.nonEmpty && existingClk.isEmpty
              val addRst = requiredRstName.nonEmpty && existingRst.isEmpty

              val opaqueDFC = DFCG()
              val clkTypeOpt: Option[coreDFOpaque[coreDFOpaque.Clk]] = clkAnnotOpt.map { clkAnnot =>
                val name = grpName(clkAnnot)
                class Unique:
                  case class Clk() extends coreDFOpaque.Clk:
                    override lazy val typeName: String = s"Clk_${name}"
                clkTypeMap.getOrElseUpdate(clkAnnot, coreDFOpaque(Unique().Clk())(using opaqueDFC))
              }
              val rstTypeOpt: Option[coreDFOpaque[coreDFOpaque.Rst]] = rstAnnotOpt.map { rstAnnot =>
                // rst is paired with its domain's clk — use the clk's grpName for naming
                // and memoization keying. If no clk slot (rare — reset without clock),
                // fall back to a synthetic "nogrp" suffix to avoid a name collision.
                val name = clkAnnotOpt.map(grpName).getOrElse("nogrp")
                class Unique:
                  case class Rst() extends coreDFOpaque.Rst:
                    override lazy val typeName: String = s"Rst_${name}"
                rstTypeMap.getOrElseUpdate(
                  (name, rstAnnot),
                  coreDFOpaque(Unique().Rst())(using opaqueDFC)
                )
              }
              // saving changed opaques to change in all relevant members later
              (existingClk, clkTypeOpt) match
                case (Some(dcl), Some(clkType)) =>
                  opaqueReplaceMap += dcl.dfType.asInstanceOf[DFOpaque] -> clkType.asIR
                case _ =>
              (existingRst, rstTypeOpt) match
                case (Some(dcl), Some(rstType)) =>
                  opaqueReplaceMap += dcl.dfType.asInstanceOf[DFOpaque] -> rstType.asIR
                case _ =>

              // add missing clk/rst ports
              if (addClk || addRst)
                val simGen = designDB.inSimulation && design.isTopTop
                val dsn = new MetaDesign(owner, Patch.Add.Config.InsideFirst):
                  val selfDFC = dfc
                  if (simGen)
                    val clk = (clkTypeOpt.get <> VAR)(using dfc.setName(requiredClkName.get))
                    lazy val rst = (rstTypeOpt.get <> VAR)(using dfc.setName(requiredRstName.get))
                    if (addRst) rst
                    val clkRstSimGen = new EDDomain:
                      override protected def __dfc: DFC =
                        selfDFC.setName("clkRstSimGen")
                          .setAnnotations(List(annotation.FlattenMode.Transparent))
                      locally {
                        given DFC = selfDFC.anonymize.setAnnotations(Nil)
                        val clkAnnot = clkAnnotOpt.get
                        val clkRate: RateNumber = clkAnnot.rate.get
                        val clkActive = clkAnnot.edge.get match
                          case ClkCfg.Edge.Rising  => true
                          case ClkCfg.Edge.Falling => false
                        val clkPeriodHalf = clkRate.to_period / 2
                        lazy val rstActive =
                          rstAnnotOpt.get.active.get match
                            case RstCfg.Active.Low  => false
                            case RstCfg.Active.High => true
                        def clkPeriodHalfConst(using
                            DFC
                        )
                            : dfhdl.core.DFConstOf[dfhdl.core.DFTime] =
                          dfhdl.core.DFVal.Const(dfhdl.core.DFTime, clkPeriodHalf)
                        process.forever {
                          if (addRst)
                            rst.actual :== dfhdl.core.DFVal.Const(dfhdl.core.DFBit, Some(rstActive))
                          val cond: Boolean <> VAL = true
                          dfhdl.core.DFWhile.plugin(cond) {
                            clk.actual :== dfhdl.core.DFVal.Const(
                              dfhdl.core.DFBit,
                              Some(!clkActive)
                            )
                            wait(clkPeriodHalfConst)
                            clk.actual :== dfhdl.core.DFVal.Const(
                              dfhdl.core.DFBit,
                              Some(clkActive)
                            )
                            wait(clkPeriodHalfConst)
                            if (addRst)
                              rst.actual :==
                                dfhdl.core.DFVal.Const(dfhdl.core.DFBit, Some(!rstActive))
                          }
                        }
                      }
                  else
                    lazy val clk = (clkTypeOpt.get <> IN)(using
                      dfc.setName(requiredClkName.get).setAnnotations(ownerClkConstraints)
                    )
                    if (addClk) clk
                    lazy val rst = (rstTypeOpt.get <> IN)(using
                      dfc.setName(requiredRstName.get)
                    )
                    if (addRst) rst
                  end if
                Some(dsn.patch)
              else None
              end if
            end if
          case _ => None
        // replace clk/rst value DFTypes with updated ones
        val opaqueTypeReplacePatches = members.view.flatMap {
          case dfVal: DFVal =>
            dfVal.dfType match
              case dfType @ DFOpaque(kind = (DFOpaque.Kind.Clk | DFOpaque.Kind.Rst))
                  if opaqueReplaceMap.contains(dfType) =>
                val updatedDFVal = dfVal match
                  case clk: DFVal.Dcl if clk.isClkDcl =>
                    val updatedAnnotations = (ownerClkConstraints ++ clk.meta.annotations).distinct
                    clk.copy(
                      dfType = opaqueReplaceMap(dfType),
                      meta = clk.meta.copy(annotations = updatedAnnotations)
                    )
                  case _ => dfVal.updateDFType(opaqueReplaceMap(dfType))
                Some(dfVal -> Patch.Replace(updatedDFVal, Patch.Replace.Config.FullReplacement))
              case _ => None
          case _ => None
        }
        // Strip only IO constraints from the owner (they moved to the clk port). Keep the
        // resolved `@timing.clock` / `@timing.reset` on the owner so that subsequent re-runs
        // of the pipeline (e.g. idempotent `.addClkRst.addClkRst`) see the resolved state
        // and don't re-derive defaults that conflict with ports already created.
        val hasOwnerIO =
          owner.meta.annotations.exists { case _: constraints.IO => true; case _ => false }
        val ownerConstraintRemovalPatchOption =
          if (hasOwnerIO)
            def updateMeta(meta: Meta): Meta =
              meta.copy(annotations = meta.annotations.flatMap {
                case _: constraints.IO => None
                case c                 => Some(c)
              })
            val updatedOwner = owner match
              case design: DFDesignBlock =>
                design.copy(meta = updateMeta(design.meta))
              case interface: DFInterfaceOwner =>
                interface.copy(
                  dclMeta = updateMeta(interface.dclMeta),
                  meta = updateMeta(interface.meta)
                )
              case _ => owner.setMeta(updateMeta)
            Some(owner -> Patch.Replace(updatedOwner, Patch.Replace.Config.FullReplacement))
          else None
        List(
          ownerConstraintRemovalPatchOption,
          opaqueTypeReplacePatches,
          ownerDomainPatchOption
        ).flatten
    }
    designDB.patch(patchList)
  end transform
end AddClkRst

extension [T: HasDB](t: T)
  def addClkRst(using CompilerOptions): DB = StageRunner.run(AddClkRst)(t.db)
