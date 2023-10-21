package dfhdl.compiler.stages

import dfhdl.compiler.analysis.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.patching.*
import DFVal.Alias.History.Op as HistoryOp
import collection.mutable

/** This stage propagates initialization values to the reg alias init value and removes the
  * initialization from the registered declaration.
  */
case object ExplicitRegInits extends Stage:
  def dependencies: List[Stage] = List()
  def nullifies: Set[Stage] = Set()
  def transform(designDB: DB)(using MemberGetSet): DB =
    val handledDcls = mutable.Set.empty[DFVal.Dcl]
    val patchList = designDB.members.flatMap {
      case ra @ DFVal.Alias.History(_, DFRef(dcl: DFVal.Dcl), _, HistoryOp.Reg, None, _, _, _) =>
        val regPatch = ra -> Patch.Replace(
          ra.copy(initOption = dcl.externalInit.map(_.head)),
          Patch.Replace.Config.FullReplacement
        )
        // A declaration may be aliased more than once, so only once we need to remove its init value
        if (!handledDcls.contains(dcl))
          handledDcls.add(dcl)
          List(
            regPatch,
            dcl -> Patch.Replace(
              dcl.removeTagOf[ExternalInit],
              Patch.Replace.Config.FullReplacement
            )
          )
        else List(regPatch)
      case _ => None
    }
    designDB.patch(patchList)
  end transform
end ExplicitRegInits

extension [T: HasDB](t: T)
  def explicitRegInits: DB =
    StageRunner.run(ExplicitRegInits)(t.db)(using dfhdl.options.CompilerOptions.default)
