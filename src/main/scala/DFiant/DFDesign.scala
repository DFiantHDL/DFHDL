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

  private[DFiant] def constructCodeString : String = designDB.addDesignCodeString(typeName, bodyCodeString, this)

  private def valCodeString : String = s"\nval $name = new $constructCodeString {}"
  final override def refCodeString(implicit callOwner: DSLOwnerConstruct): String = super.refCodeString

  override protected def discoveryDepenencies : List[Discoverable] =
    if (isTop) portsOut ++ super.discoveryDepenencies else super.discoveryDepenencies

  override def codeString: String = {
    val valCode = valCodeString
    if (isTop) s"${designDB.codeString}\n$valCode" else valCode
  }
}

object DFDesign {
  protected[DFiant] type Context = DFBlock.Context
  type ContextOf[+T] = DFBlock.ContextOf[T, DFDesign]
  private[DFiant] class DB {
    case class Info(id : Int, designs : ListBuffer[DFDesign])
    private val db = HashMap.empty[String, HashMap[String, Info]]
    def actualTypeName(designTypeName : String, info : Info) : String =
      if (info.id == 0) designTypeName else designTypeName + "$" + info.id
    def addDesignCodeString(designTypeName : String, designBodyString : String, design : DFDesign) : String = {
      val csHM = db.getOrElseUpdate(designTypeName, HashMap.empty[String, Info])
      val info = csHM.getOrElseUpdate(designBodyString, Info(csHM.size, ListBuffer.empty))
      info.designs += design
      actualTypeName(designTypeName, info)
    }
    def designTraitCodeString(designTypeName : String, designBodyString : String, info : Info) : String =
      s"\ntrait ${actualTypeName(designTypeName, info)} extends DFDesign {$designBodyString\n}"
    def codeString : String = {
      val ret = db.flatMap(e => {
        val designTypeName = e._1
        e._2.map(e => designTraitCodeString(designTypeName, e._1, e._2))
      }).mkString("\n")
      ret
    }
  }
}




