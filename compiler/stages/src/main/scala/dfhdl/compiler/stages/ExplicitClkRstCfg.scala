package dfhdl.compiler.stages

import dfhdl.compiler.analysis.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.patching.*
import dfhdl.options.CompilerOptions
import dfhdl.internals.invert
import scala.annotation.tailrec
import scala.collection.mutable
import dfhdl.core.{refTW, DFC}

/** Resolves each RT domain owner's clock/reset configuration from the user-authored partial
  * `@timing.clock` / `@timing.reset` / `@timing.related` annotations and the global
  * `DefaultRTDomainCfgTag` defaults. Writes fully-populated annotations back onto the owner's meta
  * so downstream stages (`AddClkRst`, `ToED`) can read a single, resolved source of truth.
  */
case object ExplicitClkRstCfg extends HierarchyStage:
  def dependencies: List[Stage] = List(UniqueDesigns, NamedAnonMultiref)
  def nullifies: Set[Stage] = Set(DropUnreferencedAnons)
  // resolvedClkRstMap / dependentRTDomainOwners / getOwnerDomain walk the full
  // hierarchy across sub-DB boundaries, so resolve via the outer flat-DB getSet
  // rather than per-sub-DB getSet.
  override def rebindGetSet: Boolean = false
  def transformSubDB(subDB: DB)(using
      getSet: MemberGetSet,
      co: CompilerOptions,
      rg: RefGen
  ): DB =
    val designDB = getSet.designDB
    val relatedCfgRefs = mutable.Map.empty[DFRefAny, DFMember]
    given dfc: DFC = DFC.emptyNoEO
    // Strip any @timing.clock / @timing.reset / @timing.related from the annotation
    // list — this stage will re-emit them based on the resolved result.
    def stripTimingAnnotations(
        annots: List[annotation.HWAnnotation]
    ): List[annotation.HWAnnotation] =
      annots.filter {
        case _: constraints.Timing.Clock   => false
        case _: constraints.Timing.Reset   => false
        case _: constraints.Timing.Related => false
        case _                             => true
      }

    val patchList: List[(DFMember, Patch)] = subDB.namedOwnerMemberList.flatMap {
      case (owner: (DFDomainOwner & DFBlock), members) =>
        owner.domainType match
          case DomainType.RT =>
            val (resolvedClk, resolvedRst) =
              designDB.resolvedClkRstMap.getOrElse(owner, (None, None))
            // Preserve any existing user-authored @timing.related annotation. For a domain
            // related to a sibling or to its enclosing owner the downstream pipeline uses the
            // annotation to skip clk/rst port insertion.
            val existingRelatedOpt: Option[constraints.Timing.Related] =
              owner.meta.annotations.collectFirst {
                case rel: constraints.Timing.Related => rel
              }
            // Supplement: if the domain is dependent on its enclosing owner but no @timing.related
            // was already present, synthesize one so downstream sees the relation.
            val relatedAnnotOpt: Option[constraints.Timing.Related] =
              existingRelatedOpt.orElse {
                owner match
                  case domain: DomainBlock =>
                    val domainOwner = domain.getOwnerDomain
                    if (domain.isDependentOn(domainOwner))
                      val ref =
                        domainOwner.asInstanceOf[DomainBlock | DFDesignBlock].refTW[DomainBlock]
                      relatedCfgRefs += ref -> domainOwner
                      Some(constraints.Timing.Related(ref))
                    else None
                  case _ => None
              }
            // Build new annotation list:
            //   - strip any existing timing annotations
            //   - if Related: attach only @timing.related (no clk/rst at this level)
            //   - else: attach resolved clk/rst (each Option produces 0 or 1 annotations)
            val stripped = stripTimingAnnotations(owner.meta.annotations)
            val newTimingAnnots: List[annotation.HWAnnotation] =
              relatedAnnotOpt match
                case Some(rel) => List(rel)
                case None      => resolvedClk.toList ++ resolvedRst.toList
            val newAnnots = stripped ++ newTimingAnnots
            if (newAnnots == owner.meta.annotations) None
            else
              val updatedMeta = owner.meta.copy(annotations = newAnnots)
              val updatedOwner = owner match
                case d: DomainBlock   => d.copy(meta = updatedMeta)
                case d: DFDesignBlock => d.copy(meta = updatedMeta)
              Some(owner -> Patch.Replace(updatedOwner, Patch.Replace.Config.FullReplacement))
          case _ => None
      case _ => None
    }
    if (patchList.isEmpty) subDB
    else subDB.update(refTable = subDB.refTable ++ relatedCfgRefs).patch(patchList)
  end transformSubDB
end ExplicitClkRstCfg

extension [T: HasDB](t: T)
  def explicitClkRstCfg(using CompilerOptions): DB =
    StageRunner.run(ExplicitClkRstCfg)(t.db)
