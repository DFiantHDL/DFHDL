package DFiant

import DFiant.internals._

import scala.collection.mutable.{HashMap, ListBuffer}

abstract class DFDesign(implicit ctx : DFDesign.Context) extends DFBlock with DFInterface {
  final override implicit def theOwnerToBe : DFDesign = mutableOwner.value.asInstanceOf[DFDesign]
  //The block by value object is created within the context of the current DFDesign,
  //so we mutate `theOwnerToBe` via mutableOwner which is passed to the IfBlock constructs
  private[DFiant] def injectConditionalBlock[IB <: DFDesign](ifBlock : IB, block: => Unit)(mutableOwner: MutableOwner) : IB = {
    val originalOwner = mutableOwner.value
    mutableOwner.value = ifBlock
    block
    mutableOwner.value = originalOwner
    ifBlock
  }

  private[DFiant] def designType : String = typeName
  private[DFiant] def constructCodeString : String = designDB.addOwnerBody(designType, bodyCodeString, this)

  private[DFiant] def valCodeString : String = s"\nval $name = new $constructCodeString {}"
//  final override def refCodeString(implicit callOwner: DSLOwnerConstruct): String = super.refCodeString

  override protected def discoveryDepenencies : List[Discoverable] =
    if (isTop) portsOut ++ super.discoveryDepenencies else super.discoveryDepenencies

  override def codeString: String = {
    val valCode = valCodeString
    if (isTop) s"$designDB\n$valCode" else valCode
  }
  final def printCodeString : this.type = {println(codeString); this}
  final def printVHDLString : this.type = {println(new compiler.Backend.VHDL(this)); this}
}

object DFDesign {
  protected[DFiant] type Context = DFBlock.Context
  type ContextOf[+T] = DFBlock.ContextOf[T, DFDesign]
  private[DFiant] class DB extends DSLOwnerConstruct.DB[DFDesign, String] {
    def ownerToString(designTypeName : String, designBodyString : String) : String =
      s"\ntrait $designTypeName extends DFDesign {$designBodyString\n}"
  }
}




