package dfhdl.compiler.stages

import dfhdl.compiler.analysis.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.patching.*
import dfhdl.compiler.printing.Namespacing
import dfhdl.compiler.stages.verilog.VerilogDialect
import dfhdl.options.CompilerOptions
import scala.collection.mutable
import scala.collection.immutable.ListMap

//format: off
/** Flattens the namespace-derived packages away, for a backend that has none (verilog.v95 and
  * verilog.v2001), by folding each packaged declaration's package name into its own name and
  * dropping its namespace. Everything then lands in the single global defs file, under names that
  * still say where the declaration came from and that cannot collide across packages.
  *
  * The affected declarations are exactly the ones a package-bearing backend emits into a package
  * (see `printing.Namespacing`): named types, global constants, and the global static functions /
  * ED methods emitted in the shared globals area (`analysis.HDLMethodAnalysis.globalHDLMethods`).
  * A design-local method keeps its name: it is printed inside its own design and its namespace is
  * incidental.
  *
  * ==Rule 1: a packaged named type takes its package name==
  * {{{
  * // Before (`PkgEnum` declared in the Scala package `StagesSpec.typespkg1`, top under
  * // `StagesSpec`, so the packaged emission would name it `typespkg1::PkgEnum`)
  * val e = typespkg1.PkgEnum <> VAR
  *
  * // After
  * val e = typespkg1_PkgEnum <> VAR
  * }}}
  *
  * ==Rule 2: a packaged global constant / static function takes its package name==
  * {{{
  * // Before
  * val PkgConst: UInt[8] <> CONST = d"8'42"
  * def pkgCalc(arg: UInt[8] <> CONST): UInt[8] <> CONSTRET = arg + d"8'1"
  *
  * // After
  * val typespkg1_PkgConst: UInt[8] <> CONST = d"8'42"
  * def typespkg1_pkgCalc(arg: UInt[8] <> CONST): UInt[8] <> CONSTRET = arg + d"8'1"
  * }}}
  *
  * The namespace of a flattened declaration is cleared along with the rename, so its placement
  * becomes the general global defs file and nothing is flattened twice.
  */
//format: on
case object DropPackages extends GlobalStage:
  override def runCondition(using co: CompilerOptions): Boolean =
    co.backend match
      case be: dfhdl.backends.verilog =>
        be.dialect match
          case VerilogDialect.v95 | VerilogDialect.v2001 => true
          case _                                         => false
      case _ => false
  def dependencies: List[Stage] = List()
  def nullifies: Set[Stage] = Set()

  def transformGlobal(designDB: DB)(using co: CompilerOptions, refGen: RefGen): DB =
    val topNamespace = designDB.top.dclMeta.namespace
    // the flattened name of a declaration placed in a package, `None` when it stays in the
    // general global defs file (its namespace equals or is an ancestor of the top's)
    def flattenedNameOf(ns: String, name: String): Option[String] =
      Namespacing.placementOf(ns, topNamespace).map(Namespacing.flattenedNameOf(_, name))
    def flattenMeta(meta: Meta, newName: String): Meta =
      meta.setName(newName).copy(namespace = "")

    // ---- collect the renames (deterministically, over the ordered member lists) ----
    // named types, wherever they are used (a type declared in a package is emitted into that
    // package even when a single design uses it, so design-local ones count too)
    val typeRenames = mutable.LinkedHashMap.empty[NamedDFType, String]
    // global constants
    val memberRenames = mutable.LinkedHashMap.empty[DFMember, DFMember]
    designDB.subDBs.values.foreach { sub =>
      sub.atGetSet {
        sub.members.foreach {
          case dfVal: DFVal =>
            dfVal.dfType.decompose { case dt: NamedDFType => dt }.foreach { dt =>
              Namespacing.typePlacementOf(dt, topNamespace).foreach { pkg =>
                typeRenames.getOrElseUpdate(dt, Namespacing.flattenedNameOf(pkg, dt.name))
              }
            }
          case _ =>
        }
        sub.membersGlobals.foreach { global =>
          Namespacing.placementOf(global.meta.namespace, topNamespace).foreach { pkg =>
            memberRenames.getOrElseUpdate(
              global,
              // an ANONYMOUS global (an intermediate of a global constant's expression) has no
              // name to flatten, but its namespace still has to go: it would otherwise keep
              // declaring a package of its own
              if (global.isAnonymous) global.setMeta(_.copy(namespace = ""))
              else global.setMeta(flattenMeta(_, Namespacing.flattenedNameOf(pkg, global.getName)))
            )
          }
        }
      }
    }
    // the HDL methods emitted in the shared globals area. The placement analysis reads design
    // members directly, so it runs on the FLAT view — whose design blocks are the same objects
    // as the sub-DB tops, and therefore map straight back onto the hierarchy.
    val flatDB = designDB.newToOld
    val globalMethods = flatDB.atGetSet(flatDB.globalHDLMethods)
    val designRenames = mutable.LinkedHashMap.empty[DFDesignBlock, DFDesignBlock]
    designDB.subDBs.values.foreach { sub =>
      val method = sub.top
      if (globalMethods.contains(method))
        flattenedNameOf(method.dclMeta.namespace, method.dclName).foreach { newName =>
          designRenames(method) = method.copy(meta = flattenMeta(method.meta, newName))
        }
    }

    if (typeRenames.isEmpty && memberRenames.isEmpty && designRenames.isEmpty) designDB
    else
      // ---- phase 1: the member and design-block renames, per sub-DB ----
      // A global member lives (by identity) in several sub-DB closures, so its replacement is
      // built ONCE above and reused in each of them — `newToOld` then dedups it to one member.
      // A design block is not patchable (it is its sub-DB's top and its `ownerRef` is the
      // hierarchy key, which the rename does not touch), so it is swapped in the member list
      // and in every refTable entry that resolves to it.
      val firstStepSubs: ListMap[StaticRef, DB] = ListMap.from(
        designDB.subDBs.iterator.map { (key, sub) =>
          val patched = sub.patch(sub.members.collect {
            case m if memberRenames.contains(m) =>
              m -> Patch.Replace(memberRenames(m), Patch.Replace.Config.FullReplacement)
          })
          if (designRenames.isEmpty) key -> patched
          else
            val newMembers = patched.members.map {
              case d: DFDesignBlock if designRenames.contains(d) => designRenames(d)
              case m                                             => m
            }
            val newRefTable = patched.refTable.view.mapValues {
              case d: DFDesignBlock if designRenames.contains(d) => designRenames(d)
              case t                                             => t
            }.toMap
            key -> patched.update(members = newMembers, refTable = newRefTable)
        }
      )
      val firstStep = designDB.update(subDBs = firstStepSubs)

      // ---- phase 2: the named-type renames, per sub-DB ----
      if (typeRenames.isEmpty) firstStep
      else
        val typeUpdates = typeRenames.toMap
        // built once (type rewriting reads only type structure + renames by type), keyed by
        // member so a shared global member's update is reused across every sub-DB holding it
        val typeUpdatePatches: mutable.LinkedHashMap[DFMember, (DFMember, Patch)] =
          firstStep.topDB.atGetSet {
            object ComposedNamedDFTypeReplacement
                extends ComposedDFTypeReplacement(
                  preCheck = {
                    case dt: NamedDFType => typeUpdates.get(dt)
                    case _               => None
                  },
                  updateFunc = { case (dt: NamedDFType, name) =>
                    dt.updateMeta(flattenMeta(_, name))
                  }
                )
            val patches = mutable.LinkedHashMap.empty[DFMember, (DFMember, Patch)]
            firstStep.subDBs.values.foreach { sub =>
              sub.members.foreach {
                case dfVal: DFVal =>
                  dfVal.dfType match
                    case ComposedNamedDFTypeReplacement(updatedDFType) =>
                      patches.getOrElseUpdate(
                        dfVal,
                        dfVal -> Patch.Replace(
                          dfVal.updateDFType(updatedDFType),
                          Patch.Replace.Config.FullReplacement
                        )
                      )
                    case _ =>
                case _ =>
              }
            }
            patches
          }
        val secondStepSubs: ListMap[StaticRef, DB] = ListMap.from(
          firstStep.subDBs.iterator.map { (key, sub) =>
            key -> sub.patch(sub.members.collect {
              case m if typeUpdatePatches.contains(m) => typeUpdatePatches(m)
            })
          }
        )
        firstStep.update(subDBs = secondStepSubs)
      end if
    end if
  end transformGlobal
end DropPackages

extension [T: HasDB](t: T)
  def dropPackages(using CompilerOptions): DB =
    StageRunner.run(DropPackages)(t.db)
