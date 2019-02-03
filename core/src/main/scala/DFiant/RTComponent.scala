package DFiant

import DFiant.internals._

import scala.collection.mutable.ListBuffer

abstract class RTComponent(implicit ctx0 : RTComponent.Context, args : sourcecode.Args) extends DFInterface {
  type TDev <: __Dev
  protected[DFiant] trait __Dev extends super[DFInterface].__Dev {
    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Naming
    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    final override protected def nameDefault: String = ctx.getName
    override def codeString: String = {
      s"\nval $name = new $typeName {}"
    }

    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Member discovery
    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    final override protected def discoveryDependencies : List[Discoverable] = super.discoveryDependencies ++ portsIn

    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Ownership
    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    final val id = getID
  }
  override private[DFiant] lazy val __dev : TDev = new __Dev {}.asInstanceOf[TDev]
  import __dev._

  val ctx = ctx0
  override implicit def theOwnerToBe : RTComponent = this
  protected def newGeneric() : Unit = {}
  //final protected def discovery : Unit = {}

  final protected def setInitFunc[DFVal <: DFAny.Initializable[_]](dfVal : DFVal)(value : LazyBox[Seq[dfVal.TToken]])
  : Unit = dfVal.setInitFunc.forced(value)
  final protected def getInit[DFVal <: DFAny.Initializable[_]](dfVal : DFVal) : LazyBox[Seq[dfVal.TToken]] = dfVal.initLB

  final protected[DFiant] lazy val init : Unit = {}

//  override lazy val typeName: String =
//    getClass.getName + args.value.dropRight(1).map(e => e.map(f => f.value).mkString("(",", ",")")).mkString

  val clockList : ListBuffer[Clock] = ListBuffer.empty[Clock]
  val resetList : ListBuffer[Reset] = ListBuffer.empty[Reset]
  protected[DFiant] case class Clock()(implicit n : NameIt) {
    val name : String = n.value
    clockList += this
  }
  protected[DFiant] case class Reset(activeLow : Boolean = true)(implicit n : NameIt) {
    val name : String = n.value
    resetList += this
  }
}

object RTComponent {
  type Context = DFAnyOwner.ContextOf[RTComponent, DFBlock]
}