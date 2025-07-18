package dfhdl.compiler.stages

import dfhdl.compiler.analysis.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.patching.*
import dfhdl.options.CompilerOptions
import dfhdl.compiler.stages.vhdl.VHDLDialect
import scala.collection.mutable
import dfhdl.core.DFTypeAny
import dfhdl.core.DFType.asFE

/** This stage inlines design parameter dependencies by replacing design parameters that depend on
  * other design parameters with anonymous constant values. This is needed only for VHDL'93 that
  * does not support generic parameter dependencies.
  */
case object DropDesignParamDeps extends Stage:
  override def runCondition(using co: CompilerOptions): Boolean =
    co.backend match
      case be: dfhdl.backends.vhdl =>
        be.dialect match
          case VHDLDialect.v93 => true
          case _               => false
      case _ => false
  override def dependencies: List[Stage] = List(LocalToDesignParams)
  override def nullifies: Set[Stage] = Set(DropUnreferencedAnons)
  def transform(designDB: DB)(using getSet: MemberGetSet, co: CompilerOptions): DB =
    given RefGen = RefGen.fromGetSet
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
    designDB.members.foreach {
      case param: DFVal.DesignParam =>
        param.defaultRef.get match
          case default: DFVal =>
            if (hasDesignParamDependency(default))
              designParamDefaultsToInline.add(default)
          case _ => // do nothing
      case _ => // do nothing
    }

    // Create patches to replace design parameters with anonymous constants
    val patchList: List[(DFMember, Patch)] =
      designParamDefaultsToInline.view.map { default =>
        val dsn = new MetaDesign(
          default,
          Patch.Add.Config.ReplaceWithLast(Patch.Replace.Config.FullReplacement)
        ):
          // Get the constant data from the design parameter
          val constData = default.getConstData.get
          // Create an anonymous constant with the same data
          dfhdl.core.DFVal.Const.forced(default.dfType.asFE[DFTypeAny], constData, named = false)
        dsn.patch
      }.toList

    designDB.patch(patchList)
  end transform
end DropDesignParamDeps

extension [T: HasDB](t: T)
  def dropDesignParamDeps(using co: CompilerOptions): DB =
    StageRunner.run(DropDesignParamDeps)(t.db)
