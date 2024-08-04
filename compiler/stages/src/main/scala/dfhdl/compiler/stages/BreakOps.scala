package dfhdl.compiler.stages

import dfhdl.compiler.analysis.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.patching.*
import dfhdl.options.CompilerOptions
import DFVal.Func.Op as FuncOp

/** This stage breaks the following operations:
  *   - constant index selection of anonymous vector concatenation
  *   - constant field selection of anonymous struct concatenation
  *   - assignment of anonymous vector/struct concatenation
  * Ideally this stage is run after removing user opaques and before naming multiple anonymous
  * references, to be most effective.
  */
case object BreakOps extends NoCheckStage:
  override def dependencies: List[Stage] = List(DropUserOpaques)
  override def nullifies: Set[Stage] = Set()
  def transform(designDB: DB)(using getSet: MemberGetSet, co: CompilerOptions): DB =
    object AnonConcatFuncOf:
      def unapply(dfVal: DFVal.Func): Option[List[DFVal.Ref]] =
        dfVal match
          case DFVal.Func(DFVector(_, _) | DFStruct(_, _), FuncOp.++, args, _, _, _)
              if dfVal.isAnonymous =>
            Some(args)
          case _ => None
    val patchList: List[(DFMember, Patch)] =
      designDB.members.view
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
          case net @ DFNet.Assignment(toVal, AnonConcatFuncOf(args)) =>
            toVal.dfType match
              case vectorType: DFVector =>
                val dsn = new MetaDesign(
                  net,
                  Patch.Add.Config.ReplaceWithFirst(Patch.Replace.Config.FullReplacement)
                ):
                  val toVar = toVal.asVarOf[DFType X Int]
                  for (i <- 0 until vectorType.length)
                    toVar(i) := args(i).get.asValAny
                Some(dsn.patch)
              case structType: DFStruct =>
                val dsn = new MetaDesign(
                  net,
                  Patch.Add.Config.ReplaceWithFirst(Patch.Replace.Config.FullReplacement)
                ):
                  val toVar = toVal.asVarOf[dfhdl.core.DFStruct[dfhdl.core.DFStruct.Fields]]
                  structType.fieldIndexes.foreach: (fieldName, i) =>
                    dfhdl.core.DFVal.Alias.SelectField(toVar, fieldName) := args(i).get.asValAny
                Some(dsn.patch)
              case _ => None

          case _ => None
        }
        .toList
    // recursive transformation as long as we still have something to patch
    if (patchList.nonEmpty)
      val updatedDB = designDB.patch(patchList)
      transform(updatedDB)(using updatedDB.getSet)
    else designDB
  end transform
end BreakOps

extension [T: HasDB](t: T)
  def breakOps(using co: CompilerOptions): DB =
    StageRunner.run(BreakOps)(t.db)
