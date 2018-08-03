package DFiant

import DFiant.internals._

import scala.collection.mutable.ListBuffer

abstract class RTComponent(implicit ctx : RTComponent.Context) extends DFInterface {
  override implicit def theOwnerToBe : RTComponent = this
  protected def newGeneric() : Unit = {}
  final val owner : DFBlock = ctx.owner
  final implicit val config = ctx.config

  final override protected def discoveryDepenencies : List[Discoverable] = super.discoveryDepenencies ++ portsIn
  //final protected def discovery : Unit = {}

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Naming
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  override protected def nameDefault: String = ctx.n.value
  override def toString: String = s"$fullName : $typeName"
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  final protected def setInitFunc[DFVal <: DFAny.Uninitialized](dfVal : DFVal)(value : () => Seq[dfVal.TToken])
  : Unit = dfVal.setInitFunc(value)
  final protected def getInit[DFVal <: DFAny.Uninitialized](dfVal : DFVal) : Seq[dfVal.TToken] = dfVal.getInit

  final protected[DFiant] lazy val init : Unit = {
    //set Output Ports Dependency
//    portNodes.map(pn => pn.dfport).filter(p => p.dir.isOut).foreach(p => p.setComponentDependency(this))
  }
  final val id = getID

  override lazy val typeName: String = getClass.getName

  override def codeString: String = {
    s"\nval $name = new $typeName {}"
  }
}

object RTComponent {
  type Context = DFAnyOwner.ContextOf[RTComponent, DFBlock]
}