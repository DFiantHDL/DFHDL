package dfhdl.compiler.stages

import dfhdl.compiler.analysis.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.patching.*
import dfhdl.options.CompilerOptions

/** This stage drops physical values by inlining them
  */
case object DropPhysicalValues extends Stage:
  override def dependencies: List[Stage] = List()
  override def nullifies: Set[Stage] = Set()
  def transform(designDB: DB)(using getSet: MemberGetSet, co: CompilerOptions): DB =
    val keepProcessDcls = co.backend.isVHDL
    val patchList: List[(DFMember, Patch)] =
      designDB.members.collect {
        case alias @ DFVal.Alias.AsIs(dfType, DFRef(fromVal), ownerRef, meta, tags)
            if fromVal.dfType == DFNumber =>
          val data = alias.getConstData.get
          val const = DFVal.Const(dfType, data, ownerRef, meta, tags)
          alias -> Patch.Replace(const, Patch.Replace.Config.FullReplacement)
        case dfVal: DFVal if dfVal.dfType.isInstanceOf[DFPhysical[?]] =>
          dfVal -> Patch.Remove()
      }.toList
    designDB.patch(patchList)
  end transform
end DropPhysicalValues

extension [T: HasDB](t: T)
  def dropPhysicalValues(using co: CompilerOptions): DB =
    StageRunner.run(DropPhysicalValues)(t.db)
