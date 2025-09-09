package dfhdl.compiler.stages

import dfhdl.compiler.analysis.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.patching.*
import dfhdl.options.CompilerOptions

/** This stage drops physical values by inlining them. For wait statements, their values are inlined
  * as anonymous constants.
  */
case object DropPhysicalValues extends Stage:
  override def dependencies: List[Stage] = List()
  override def nullifies: Set[Stage] = Set()
  def transform(designDB: DB)(using getSet: MemberGetSet, co: CompilerOptions): DB =
    val keepProcessDcls = co.backend.isVHDL
    val patchList: List[(DFMember, Patch)] =
      designDB.members.flatMap {
        // cast of number to int/double is inlined as a constant
        case alias @ DFVal.Alias.AsIs(dfType, DFRef(fromVal), ownerRef, meta, tags)
            if fromVal.dfType == DFNumber =>
          val data = alias.getConstData.get
          val const = DFVal.Const(dfType, data, ownerRef, meta, tags)
          Some(alias -> Patch.Replace(const, Patch.Replace.Config.FullReplacement))
        // the rest of physical values are removed if they are not used in a wait statement.
        // otherwise, they are inlined as anonymous constants
        case dfVal: DFVal if dfVal.dfType.isInstanceOf[DFPhysical[?]] =>
          val keep = dfVal.originMembersNoTypeRef.exists {
            case _: Wait => true
            case _       => false
          }
          // an anonymous time referenced by a wait statement is kept
          if (keep && dfVal.isAnonymous) None
          // referenced by a wait statement, but not anonymous, so we inline it as an anonymous constant
          else if (keep)
            val const = DFVal.Const(
              dfVal.dfType, dfVal.getConstData.get, dfVal.ownerRef, dfVal.meta.anonymize, dfVal.tags
            )
            Some(dfVal -> Patch.Replace(const, Patch.Replace.Config.FullReplacement))
          // not referenced by a wait statement, so we remove it
          else
            Some(dfVal -> Patch.Remove())
        case _ => None
      }
    designDB.patch(patchList)
  end transform
end DropPhysicalValues

extension [T: HasDB](t: T)
  def dropPhysicalValues(using co: CompilerOptions): DB =
    StageRunner.run(DropPhysicalValues)(t.db)
