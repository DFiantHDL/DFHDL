package DFiant
package compiler

import DFDesign.DB.Patch
import DFiant.DFAny.Alias
import analysis.DFAnyAnalysis

final class NamedAliases[D <: DFDesign](c: IRCompilation[D]) {
  private val designDB = c.db
  import designDB.__getset

  //Names an anonymous relative value which is aliased.
  //The aliasing is limited according to the criteria provided
  def namedAliases(criteria : DFAny.Alias => Boolean) : IRCompilation[D] = {
    val membersToName = designDB.members.collect {
      case alias : DFAny.Alias if alias.relValRef.get.isAnonymous && criteria(alias) => alias.relValRef.get
    }.filter {
      case _ : DFAny.Const => false //ignore constants
      case _ : DFAny.Alias.Prev => false  //previous values will get proper names in another stage
      case _ : DFAny.ApplySel => false //ignore apply sel
      case _ => true
    }
    //we force set the underlying original name before it was anonymized
    val patchList = membersToName.map(m =>
      m -> (Patch.Replace(
        m.setName(m.suggestName),
        Patch.Replace.Config.FullReplacement
      ))
    )
    c.newStage(designDB.patch(patchList))
  }
  private def namedSelectionCriteria: DFAny.Alias => Boolean = {
    case Alias.BitsWL.Unref(_, _, relVal, relWidth, _, _, _)
        if relWidth != relVal.width =>
      true
    case alias: Alias.AsIs if alias.width != alias.relValRef.get.width => true
    case _                                                             => false
  }
  //For verilog simulation in verilator (and possibly other tools), bit selection from unnamed values is limited.
  //This compilation stage names the intermediate values. A future stage (UniqueNames) is responsible for
  //making sure the names will be unique.
  def namedSelection: IRCompilation[D] = namedAliases(namedSelectionCriteria)
  private def namedPrevCriteria: DFAny.Alias => Boolean = {
    case _: Alias.Prev => true
    case _             => false
  }
  //Creating a previous values of a value requires that value to be names to avoid random anonymous names in the
  //the backend
  def namedPrev: IRCompilation[D] = namedAliases(namedPrevCriteria)
}
