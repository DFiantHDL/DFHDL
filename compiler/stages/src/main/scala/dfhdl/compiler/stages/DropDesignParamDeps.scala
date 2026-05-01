package dfhdl.compiler.stages

import dfhdl.compiler.analysis.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.patching.*
import dfhdl.options.CompilerOptions
import dfhdl.compiler.stages.vhdl.VHDLDialect
import scala.collection.mutable
import dfhdl.core.DFTypeAny
import dfhdl.core.DFType.asFE
import dfhdl.compiler.ir.ConstData.CachePolicy

/** This stage inlines design parameter dependencies by replacing design parameters that depend on
  * other design parameters with anonymous constant values. This is needed only for VHDL'93 that
  * does not support generic parameter dependencies.
  */
case object DropDesignParamDeps extends HierarchyStage:
  override def runCondition(using co: CompilerOptions): Boolean =
    co.backend match
      case be: dfhdl.backends.vhdl =>
        be.dialect match
          case VHDLDialect.v93 => true
          case _               => false
      case _ => false
  override def dependencies: List[Stage] = List(LocalToDesignParams)
  override def nullifies: Set[Stage] = Set(DropUnreferencedAnons)
  // DesignParams referenced via `hasDesignParamDependency` can resolve to
  // params in other sub-DBs; use outer flat getSet for cross-hierarchy refs.
  override def rebindGetSet: Boolean = false
  def transformSubDB(subDB: DB)(using getSet: MemberGetSet, co: CompilerOptions, rg: RefGen): DB =
    val designParamDefaultsToInline = mutable.LinkedHashSet.empty[DFVal]

    // Check if a design parameter depends on other design parameters
    def hasDesignParamDependency(dfVal: DFVal): Boolean =
      dfVal.getRefs.exists { ref =>
        ref.get match
          case depParam: DFVal.DesignParam => true
          case depVal: DFVal               => hasDesignParamDependency(depVal)
          case _                           => false
      }

    // Collect all design parameters that have dependencies on other design parameters
    subDB.members.foreach {
      case param: DFVal.DesignParam =>
        param.defaultValRef.get match
          case default: DFVal =>
            if (hasDesignParamDependency(default))
              designParamDefaultsToInline.add(default)
          case _ => // do nothing
      case _ => // do nothing
    }

    // Create patches to replace design parameters with anonymous constants
    val patches = designParamDefaultsToInline.view.map { default =>
      // Get the constant data from the design parameter
      val constData = default.getConstDataOrDefault[Any]
      val dsn = new MetaDesign(
        default,
        Patch.Add.Config.ReplaceWithLast(Patch.Replace.Config.FullReplacement)
      ):
        // Create an anonymous constant with the same data
        dfhdl.core.DFVal.Const.forced(default.dfType.asFE[DFTypeAny], constData, named = false)
      dsn.patch
    }.toList
    subDB.patch(patches)
  end transformSubDB
end DropDesignParamDeps

extension [T: HasDB](t: T)
  def dropDesignParamDeps(using co: CompilerOptions): DB =
    StageRunner.run(DropDesignParamDeps)(t.db)
