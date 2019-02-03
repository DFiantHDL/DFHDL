package DFiant

import DFiant.BasicLib.DFBasicLib
import DFiant.compiler.Backend
import DFiant.internals._

abstract class DFDesign(implicit ctx : DFDesign.Context) extends DFBlock with DFInterface {
  type TDev <: __Dev
  protected[DFiant] trait __Dev extends super[DFBlock].__Dev with super[DFInterface].__Dev {
    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Member discovery
    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    override protected def discoveryDependencies : List[Discoverable] =
      if (isTop) portsOut ++ super.discoveryDependencies else super.discoveryDependencies
  }
  override private[DFiant] lazy val __dev : TDev = new __Dev {}.asInstanceOf[TDev]
  import __dev._

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
  protected def atOwnerDo[T](block : => T) : T = {
    val originalOwner = mutableOwner.value
    mutableOwner.value = owner.asInstanceOf[DFBlock]
    val ret = block
    mutableOwner.value = originalOwner
    ret
  }

  final lazy val isTop : Boolean = __dev.isTop

  private[DFiant] def designType : String = typeName
  private[DFiant] def constructCodeString : String = designDB.addOwnerBody(designType, bodyCodeString, this)

  private[DFiant] def valCodeString : String = s"\nval $name = new $constructCodeString {}"
//  final override def refCodeString(implicit callOwner: DSLOwnerConstruct): String = super.refCodeString


  override def codeString: String = {
    init
    val valCode = valCodeString
    if (isTop) s"$designDB\n$valCode" else valCode
  }
  private def openInputsCheck() : Unit = discoveredList.collect {
    case p : DFAny.Port[_,_] if p.dir.isIn && !isTop && !p.isConnected && p.initLB.get.isEmpty =>
      throw new IllegalArgumentException(s"\nFound an uninitialized open input port: ${p.fullName}")
  }
  private lazy val init : Unit = {
    openInputsCheck()
  }
  final def printCodeString : this.type = {println(codeString); this}
  def compileToVHDL : Backend.VHDL = {init; new Backend.VHDL(this)}
  final def printVHDLString : this.type = {compileToVHDL.print(); this}
}

object DFDesign {
  private[DFiant] type Context = DFBlock.Context
  trait ContextOf[+T] {
    val ownerOption : Option[DFBlock]
    val basicLib: DFBasicLib
    val config : DFAnyConfiguration
    val n : NameIt
  }
  object ContextOf {
    implicit def ev[T](
      implicit
      evOwner : DFBlock = null,
      evBasicLib : DFBasicLib,
      evConfig : DFAnyConfiguration,
      evNameIt : NameIt,
      forceNotVar : NameIt.ForceNotVar[ContextOf[_]]
    ) : ContextOf[T] = new ContextOf[T] {
      val ownerOption : Option[DFBlock] = Option(evOwner)
      val basicLib: DFBasicLib = evBasicLib
      val config: DFAnyConfiguration = evConfig
      val n: NameIt = evNameIt
    }
  }
  private[DFiant] class DB extends DSLOwnerConstruct.DB[DFDesign, String] {
    def ownerToString(designTypeName : String, designBodyString : String) : String =
      s"\ntrait $designTypeName extends DFDesign {$designBodyString\n}"
  }
}




