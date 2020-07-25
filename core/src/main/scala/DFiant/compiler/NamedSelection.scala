package DFiant
package compiler

import DFDesign.DB.Patch

import scala.annotation.tailrec
import scala.reflect.{ClassTag, classTag}

//For verilog simulation in verilator (and possibly other tools), bit selection from unnamed values is limited.
//This compilation stage names the intermediate values. A future stage (UniqueNames) is responsible for
//making sure the names will be unique.
final class NamedSelection[D <: DFDesign](c : IRCompilation[D]) {
  private val designDB = c.db
  import designDB.__getset
  def namedSelection : IRCompilation[D] = {
    val membersToName = designDB.members.collect {
      case DFAny.Alias.BitsWL.Unref(_,_,relVal,relWidth,_,_,_) if relVal.isAnonymous && relWidth != relVal.width.getValue => relVal
//      case DFAny.Alias.AsIs.Unref(_,_,relVal,_,_) if relVal.isAnonymous => relVal
      case DFAny.Alias.Resize.Unref(_,relVal,_,_) if relVal.isAnonymous => relVal
    }.filter {
      case _ : DFAny.Const => false
      case _ => true
    }
    //we force set the underlying original name before it was anonymized
    val patchList = membersToName.map(m => m -> (Patch.Replace(m.setName(m.tags.meta.name.value), Patch.Replace.Config.FullReplacement)))
    c.newStage(designDB.patch(patchList))
  }
}