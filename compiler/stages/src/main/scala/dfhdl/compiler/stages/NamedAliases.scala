package dfhdl.compiler.stages

import dfhdl.compiler.analysis.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.patching.*
import DFVal.Func.Op as FuncOp

// Names an anonymous relative value which is aliased.
// The aliasing is limited according to the criteria provided
private abstract class NamedAliases(criteria: NamedAliases.Criteria) extends Stage:
  override def dependencies: List[Stage] = Nil
  override def nullifies: Set[Stage] = Set(DFHDLUniqueNames, DropLocalDcls)

  def transform(designDB: DB)(using MemberGetSet): DB =

    val patchList =
      designDB.members.view
        .collect { case dfVal: DFVal => dfVal }
        .flatMap { dfVal => criteria()(dfVal) }.filter {
          case _: DFVal.Const => false // ignore constants
          case _: DFVal.Alias.History =>
            false // history values will get proper names in another stage
          case _: DFVal.Alias.ApplyIdx    => false // ignore index selection
          case _: DFVal.Alias.SelectField => false // ignore field selection
          case _                          => true
        }.map { m =>
          // we force set the underlying original name before it was anonymized
          val namedMember = m.setName(m.suggestName.getOrElse("anon"))
          m -> Patch.Replace(
            namedMember,
            Patch.Replace.Config.FullReplacement
          )
        }.toList
    designDB.patch(patchList)
  end transform
end NamedAliases

object NamedAliases:
  trait Criteria:
    def apply()(using MemberGetSet): DFVal => List[DFVal]
  object Criteria:
    private val carryOps = Set(FuncOp.`*`, FuncOp.+, FuncOp.-)
    extension (dfVal: DFVal)(using MemberGetSet)
      def hasVerilogName: Boolean =
        dfVal match
          case dfVal if !dfVal.isAnonymous => true
          case alias: DFVal.Alias.AsIs =>
            val relVal = alias.relValRef.get
            val transparentConversion = (alias.dfType, relVal.dfType) match
              case (DFUInt(toWidth), DFBits(fromWidth)) => toWidth == fromWidth
              case (DFBits(toWidth), DFUInt(fromWidth)) => toWidth == fromWidth
              case (DFBit, DFBool)                      => true
              case (DFBool, DFBit)                      => true
              case _                                    => false
            if (transparentConversion) relVal.hasVerilogName
            else false
          case _ => false
    object NamedSelection extends Criteria:
      def apply()(using MemberGetSet): DFVal => List[DFVal] = {
        case alias: DFVal.Alias if alias.relValRef.get.hasVerilogName => Nil
        case alias: DFVal.Alias.ApplyRange if alias.width != alias.relValRef.get.width =>
          List(alias.relValRef.get)
        case alias: DFVal.Alias.AsIs if alias.width < alias.relValRef.get.width =>
          List(alias.relValRef.get)
        case alias: DFVal.Alias.ApplyIdx =>
          List(alias.relValRef.get)
        case func @ DFVal.Func(_, op, DFRef(lhs) :: _ :: Nil, _, _, _)
            if !lhs.hasVerilogName && carryOps.contains(op) && func.width > lhs.width =>
          List(lhs)
        case _ => Nil
      }
    end NamedSelection
    object NamedPrev extends Criteria:
      def apply()(using MemberGetSet): DFVal => List[DFVal] = {
        case alias: DFVal.Alias.History if alias.relValRef.get.isAnonymous =>
          List(alias.relValRef.get)
        case _ => Nil
      }
  end Criteria
end NamedAliases

// For verilog simulation in verilator (and possibly other tools), bit selection from unnamed values is limited.
// This compilation stage names the intermediate values. A future stage (UniqueNames) is responsible for
// making sure the names will be unique.
case object NamedSelection extends NamedAliases(NamedAliases.Criteria.NamedSelection)
extension [T: HasDB](t: T) def namedSelection: DB = StageRunner.run(NamedSelection)(t.db)

// Creating a previous values of a value requires that value to be names to avoid random anonymous names in the
// the backend
case object NamedPrev extends NamedAliases(NamedAliases.Criteria.NamedPrev)
extension [T: HasDB](t: T) def namedPrev: DB = StageRunner.run(NamedPrev)(t.db)
