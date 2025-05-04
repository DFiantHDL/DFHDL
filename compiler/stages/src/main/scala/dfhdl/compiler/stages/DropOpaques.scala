package dfhdl.compiler.stages

import dfhdl.compiler.analysis.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.patching.*
import dfhdl.options.CompilerOptions
import scala.collection.immutable.ListMap
import dfhdl.compiler.stages.verilog.VerilogDialect

/** This stage drops all opaque types that fit within the applied filter predicate.
  * @param filterPred
  */
abstract class DropOpaques(filterPred: DFOpaque => Boolean) extends Stage:
  override def dependencies: List[Stage] = List()
  override def nullifies: Set[Stage] = Set()
  def transform(designDB: DB)(using MemberGetSet, CompilerOptions): DB =
    object ComposedOpaqueDFTypeReplacement
        extends ComposedDFTypeReplacement(
          preCheck = {
            case dt: DFOpaque if filterPred(dt) => Some(())
            case _                              => None
          },
          updateFunc = { case (dt: DFOpaque, _) => dt.actualType }
        )
    object ComposedOpaqueDFValReplacement:
      def unapply(dfVal: DFVal): Option[DFVal] = dfVal.dfType match
        case ComposedOpaqueDFTypeReplacement(dfType) => Some(dfVal.updateDFType(dfType))
        case _                                       => None
    val patchList: List[(DFMember, Patch)] =
      designDB.members.view
        .flatMap {
          // casting to/from an opaque needs to be removed
          case dfVal: DFVal.Alias.AsIs =>
            val relVal = dfVal.relValRef.get
            (dfVal.dfType, relVal.dfType) match
              // `x.as(opaque)` cast is changed to just `x`
              case (toType @ DFOpaque(actualType = at), fromType)
                  if at == fromType && filterPred(toType) =>
                Some(dfVal -> Patch.Replace(relVal, Patch.Replace.Config.ChangeRefAndRemove))
              // `opaqueX.actual` cast is changed to just `opaqueX`. The `opaqueX` member is changed at a different case.
              case (toType, fromType @ DFOpaque(actualType = at))
                  if at == toType && filterPred(fromType) =>
                Some(dfVal -> Patch.Replace(relVal, Patch.Replace.Config.ChangeRefAndRemove))
              // otherwise, the values should be checked for composed opaques and changed accordingly
              case _ =>
                dfVal match
                  case dfVal @ ComposedOpaqueDFValReplacement(updatedDFVal) =>
                    Some(dfVal -> Patch.Replace(updatedDFVal, Patch.Replace.Config.FullReplacement))
                  case _ => None
            end match
          // all other opaque values need to be changed to their actual types
          case dfVal @ ComposedOpaqueDFValReplacement(updatedDFVal) =>
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
      case DFOpaque(kind = _: DFOpaque.Kind.Magnet) => true
      case _                                        => false
    }):
  override def dependencies: List[Stage] = List(ConnectMagnets)

case object DropUserOpaques
    extends DropOpaques({
      case DFOpaque(kind = _: DFOpaque.Kind.Magnet) => false
      case _                                        => true
    }),
      NoCheckStage:
  override def runCondition(using co: CompilerOptions): Boolean =
    val unsupportedInBackend = co.backend match
      case be: dfhdl.backends.verilog =>
        be.dialect match
          case VerilogDialect.v95 | VerilogDialect.v2001 => true
          case _                                         => false
      case _ => false
    co.dropUserOpaques || unsupportedInBackend

extension [T: HasDB](t: T)
  def dropOpaques(using CompilerOptions): DB =
    StageRunner.run(DropOpaquesAll)(t.db)

extension [T: HasDB](t: T)
  def dropMagnets(using CompilerOptions): DB =
    StageRunner.run(DropMagnets)(t.db)
