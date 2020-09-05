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
  @tailrec def suggestName(member : DFAny.Member, prevMember : Option[DFAny.Member] = None) : String =
    designDB.memberTable(member)
      .view.map(r => (r, r.refType))
      .collectFirst {
        //search consuming references
        case (r : DFMember.OwnedRef, _ : DFAny.Ref.ConsumeFrom.Type) => r.owner.get
        //search aliasing references, as long as we don't go back to previous member
        //(aliasing can be used for both producing and consuming)
        case (r : DFMember.OwnedRef, _ : DFAny.Alias.RelValRef.Type)
          if prevMember.isEmpty || prevMember.get != r.owner.get  => r.owner.get
      } match {
      //found yet another anonymous value -> continue searching
      case Some(value : DFAny.Member) if value.isAnonymous => suggestName(value, Some(member))
      //name from a named value which was referenced by an alias
      case Some(value : DFAny.Member) => s"${value.name}_part"
      //name from assignment or connection
      case Some(net : DFNet) => s"${net.toRef.get.name}_part"
      case _ => member match {
        //found an alias -> checking its relative value
        case a : DFAny.Alias =>
          val relVal = a.relValRef.get
          if (relVal.isAnonymous) suggestName(relVal, Some(member))
          else s"${relVal.name}_part"
        //no named source found -> relying on the default anonymous naming
        case _ => member.name
      }
    }

  def namedSelection : IRCompilation[D] = {
    val membersToName = designDB.members.collect {
      case DFAny.Alias.BitsWL.Unref(_,_,relVal,relWidth,_,_,_) if relVal.isAnonymous && relWidth != relVal.width => relVal
      case DFAny.Alias.Resize.Unref(_,relVal,_,_) if relVal.isAnonymous => relVal
    }.filter {
      case _ : DFAny.Const => false //ignore constants
      case _ : DFAny.Alias.Prev => false  //previous values will get proper names in another stage
      case _ => true
    }
    //we force set the underlying original name before it was anonymized
    val patchList = membersToName.map(m => m -> (Patch.Replace(m.setName(suggestName(m)), Patch.Replace.Config.FullReplacement)))
    c.newStage(designDB.patch(patchList))
  }
}