package dfhdl.compiler.stages

import dfhdl.compiler.analysis.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.patching.*
import dfhdl.options.CompilerOptions
import DFVal.Func.Op as FuncOp
import scala.annotation.tailrec

/** This stage breaks the following operations:
  *   - constant index selection of anonymous vector concatenation
  *   - constant field selection of anonymous struct concatenation
  *   - assignment of anonymous vector/struct concatenation (conditioned upon `breakAssignments`)
  * Ideally this stage is run after removing user opaques and before naming multiple anonymous
  * references, to be most effective. Without removing user opaques first, we pass through anonymous
  * to/from opaque casting while looking for the concatenation.
  */
abstract class BreakOps(breakAssignments: Boolean) extends HierarchyStage, NoCheckStage:
  override def dependencies: List[Stage] = List()
  override def nullifies: Set[Stage] = Set(DropUnreferencedAnons)
  // iterate transform until fixpoint (no patches produced)
  override def transform(designDB: DB)(using MemberGetSet, CompilerOptions): DB =
    val result = super.transform(designDB)
    if (result eq designDB) designDB
    else transform(result)(using result.getSet)
  def transformSubDB(subDB: DB)(using MemberGetSet, CompilerOptions, RefGen): DB =
    object AnonConcatFuncOf:
      @tailrec def unapply(dfVal: DFVal): Option[List[DFVal.Ref]] =
        if (dfVal.isAnonymous) dfVal match
          case DFVal.Func(dfType = DFVector(_, _) | DFStruct(_, _), op = FuncOp.++, args = args) =>
            Some(args)
          // pass through opaque to/from casting
          case OpaqueActual(relVal) => unapply(relVal)
          case AsOpaque(relVal)     => unapply(relVal)
          case _                    => None
        else None
    val patches = subDB.members.view
      .flatMap {
          // constant index selection from a concat
          case dfVal @ DFVal.Alias.ApplyIdx(
                _,
                DFRef(AnonConcatFuncOf(args)),
                DFRef(DFVal.Alias.ApplyIdx.ConstIdx(idx)),
                _,
                _,
                _
              ) =>
            Some(dfVal -> Patch.Replace(args(idx).get, Patch.Replace.Config.ChangeRefAndRemove))
          // constant field selection from a concat
          case dfVal @ DFVal.Alias.SelectField(
                _,
                DFRef(relVal @ AnonConcatFuncOf(args)),
                fieldName,
                _,
                _,
                _
              ) =>
            val idx = relVal.dfType.asInstanceOf[DFStruct].fieldIndex(fieldName)
            Some(dfVal -> Patch.Replace(args(idx).get, Patch.Replace.Config.ChangeRefAndRemove))
          // assignments (blocking/non-blocking) breaking
          case net @ DFNet.Assignment(toVal, AnonConcatFuncOf(args)) if breakAssignments =>
            import dfhdl.core.{assign, nbassign}
            toVal.dfType match
              case vectorType: DFVector =>
                val dsn = new MetaDesign(
                  net,
                  Patch.Add.Config.ReplaceWithFirst(Patch.Replace.Config.FullReplacement)
                ):
                  val toVar = toVal.asVarOf[DFType X Int]
                  // we assume concatenated vectors have known widths
                  val length = vectorType.lengthIntOpt.get
                  for (i <- 0 until length)
                    val lhs = toVar(i)
                    val rhs = args(i).get.asValAny
                    if (net.op == DFNet.Op.Assignment)
                      lhs.assign(rhs)
                    else
                      lhs.nbassign(rhs)
                Some(dsn.patch)
              case structType: DFStruct =>
                val dsn = new MetaDesign(
                  net,
                  Patch.Add.Config.ReplaceWithFirst(Patch.Replace.Config.FullReplacement)
                ):
                  val toVar = toVal.asVarOf[dfhdl.core.DFStruct[dfhdl.core.DFStruct.Fields]]

                  structType.fieldIndexes.foreach: (fieldName, i) =>
                    val lhs = dfhdl.core.DFVal.Alias.SelectField(toVar, fieldName)
                    val rhs = args(i).get.asValAny
                    if (net.op == DFNet.Op.Assignment)
                      lhs.assign(rhs)
                    else
                      lhs.nbassign(rhs)
                Some(dsn.patch)
              case _ => None
            end match

          case _ => None
        }
        .toList
    subDB.patch(patches)
  end transformSubDB
end BreakOps

case object BreakOpsNoAssignments extends BreakOps(breakAssignments = false)
case object BreakOpsWithAssignments extends BreakOps(breakAssignments = true)

extension [T: HasDB](t: T)
  def breakOpsWithAssignments(using co: CompilerOptions): DB =
    StageRunner.run(BreakOpsWithAssignments)(t.db)
