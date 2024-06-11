package dfhdl.compiler.stages

import dfhdl.compiler.analysis.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.patching.*
import dfhdl.options.CompilerOptions

/** This stage drops all opaque types that fit within the applied filter predicate. TODO: currently
  * not handling opaques that are composed.
  * @param filterPred
  */
abstract class DropOpaques(filterPred: DFOpaque => Boolean) extends Stage:
  override def dependencies: List[Stage] = List()
  override def nullifies: Set[Stage] = Set()
  def transform(designDB: DB)(using MemberGetSet, CompilerOptions): DB =
    val patchList: List[(DFMember, Patch)] =
      designDB.members.view
        .flatMap {
          // casting to/from an opaque needs to be removed
          case dfVal: DFVal.Alias.AsIs =>
            val relVal = dfVal.relValRef.get
            (dfVal.dfType, relVal.dfType) match
              // `x.as(opaque)` cast is changed to just `x`
              case (toType @ DFOpaque(_, _, at), fromType)
                  if at == fromType && filterPred(toType) =>
                Some(dfVal -> Patch.Replace(relVal, Patch.Replace.Config.ChangeRefAndRemove))
              // `opaqueX.actual` cast is changed to just `opaqueX`. The `opaqueX` member is changed at a different case.
              case (toType, fromType @ DFOpaque(_, _, at))
                  if at == toType && filterPred(fromType) =>
                Some(dfVal -> Patch.Replace(relVal, Patch.Replace.Config.ChangeRefAndRemove))
              case _ => None
          // all other opaque values need to be changed to their actual types
          case dfVal @ DFOpaque.Val(dfType) if filterPred(dfType) =>
            import dfType.actualType
            val updatedDFVal = dfVal.updateDFType(dfType = actualType)
            Some(dfVal -> Patch.Replace(updatedDFVal, Patch.Replace.Config.FullReplacement))
          case _ => None
        }
        .toList
    designDB.patch(patchList)
  end transform
end DropOpaques

//This stage drops all opaque types
case object DropOpaquesAll extends DropOpaques(_ => true)

//This stage drops all magnet types
case object DropMagnets
    extends DropOpaques({
      case DFOpaque(_, _: DFOpaque.MagnetId, _) => true
      case _                                    => false
    }):
  override def dependencies: List[Stage] = List(ConnectMagnets)

extension [T: HasDB](t: T)
  def dropMagnets(using CompilerOptions): DB =
    StageRunner.run(DropMagnets)(t.db)
