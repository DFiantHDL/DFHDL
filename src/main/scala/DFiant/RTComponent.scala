package DFiant

import DFiant.internals._

import scala.collection.mutable.ListBuffer

abstract class RTComponent(implicit ctx : RTComponent.Context, args : sourcecode.Args) extends DFInterface {
  override implicit def theOwnerToBe : RTComponent = this
  protected def newGeneric() : Unit = {}
  final val owner : DFBlock = ctx.owner
  final implicit val config = ctx.config

  final override protected def discoveryDepenencies : List[Discoverable] = super.discoveryDepenencies ++ portsIn
  //final protected def discovery : Unit = {}

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Naming
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  final override private[DFiant] def nameDefault: String = ctx.n.value
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  final protected def setInitFunc[DFVal <: DFAny.Uninitialized](dfVal : DFVal)(value : => Seq[dfVal.TToken])
  : Unit = dfVal.setInitFunc(value)
  final protected def getInit[DFVal <: DFAny.Uninitialized](dfVal : DFVal) : Seq[dfVal.TToken] = dfVal.getInit

  final protected[DFiant] lazy val init : Unit = {}
  final val id = getID

  override lazy val typeName: String =
    getClass.getName + args.value.dropRight(1).map(e => e.map(f => f.value).mkString("(",", ",")")).mkString

  override def codeString: String = {
    s"\nval $name = new $typeName {}"
  }
}

object RTComponent {
  type Context = DFAnyOwner.ContextOf[RTComponent, DFBlock]
}