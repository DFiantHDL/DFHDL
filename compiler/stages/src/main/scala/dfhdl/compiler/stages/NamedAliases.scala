package dfhdl.compiler.stages

import dfhdl.compiler.analysis.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.patching.*

// Names an anonymous relative value which is aliased.
// The aliasing is limited according to the criteria provided
private abstract class NamedAliases(criteria: NamedAliases.Criteria) extends Stage:
  override def dependencies: List[Stage] = Nil
  override def nullifies: Set[Stage] = Set(DFHDLUniqueNames, NoLocalDcls)

  def transform(designDB: DB)(using MemberGetSet): DB =

    val membersToName = designDB.members.collect {
      case alias: DFVal.Alias if alias.relValRef.get.isAnonymous && criteria()(alias) =>
        alias.relValRef.get
    }.filter {
      case _: DFVal.Const          => false // ignore constants
      case _: DFVal.Alias.History  => false // history values will get proper names in another stage
      case _: DFVal.Alias.ApplyIdx => false // ignore index selection
      case _                       => true
    }
    // we force set the underlying original name before it was anonymized
    val patchList = membersToName.map(m =>
      m -> (Patch.Replace(
        m.setName(m.suggestName.getOrElse("anon")),
        Patch.Replace.Config.FullReplacement
      ))
    )
    designDB.patch(patchList)
  end transform
end NamedAliases

object NamedAliases:
  trait Criteria:
    def apply()(using MemberGetSet): DFVal.Alias => Boolean
  object Criteria:
    object NamedSelection extends Criteria:
      def apply()(using MemberGetSet): DFVal.Alias => Boolean = {
        case applyRange: DFVal.Alias.ApplyRange =>
          applyRange.width != applyRange.relValRef.get.width
        case alias: DFVal.Alias.AsIs =>
          alias.width < alias.relValRef.get.width
        case _: DFVal.Alias.ApplyIdx => true
        case _                       => false
      }
    object NamedPrev extends Criteria:
      def apply()(using MemberGetSet): DFVal.Alias => Boolean = {
        case _: DFVal.Alias.History => true
        case _                      => false
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
