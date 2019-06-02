package DFiant

import DFiant.internals._

import scala.collection.mutable.ListBuffer

abstract class RTComponent(implicit ctx0 : RTComponent.Context, args : sourcecode.Args) extends DFInterface {
  final private[DFiant] lazy val ctx = ctx0
  protected[DFiant] trait __DevRTComponent extends __DevDFInterface {
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
  }
  override private[DFiant] lazy val __dev : __DevRTComponent = new __DevRTComponent {}
  import __dev._

  override implicit def __theOwnerToBe : RTComponent = this
  protected def newGeneric() : Unit = {}
  //final protected def discovery : Unit = {}

  final protected def setInitFunc[DFVal <: DFAny.Initializable[_]](dfVal : DFVal)(value : LazyBox[Seq[DFVal#TToken]])
  : Unit = dfVal.setInitFunc.forced(value)
  final protected def getInit[DFVal <: DFAny](dfVal : DFVal) : LazyBox[Seq[dfVal.TToken]] = dfVal.initLB

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

//class RTOp2[L <: DFAny, R <: DFAny, O <: DFAny.Initializable[_]](val O : O, val L : L, val R : R)
//  (func : (Seq[L#TToken], Seq[R#TToken]) => Seq[O#TToken])
//  (implicit ctx : RTComponent.Context) extends RTComponent {
//  setInitFunc(O)(LazyBox.Args2[Seq[O#TToken], Seq[L#TToken], Seq[R#TToken]](this)(func, getInit(L), getInit(R)))
//}

sealed abstract class RTOp2(implicit ctx : RTComponent.Context) extends RTComponent { self =>
  val O : DFAny.Var //Output variable
  val L : DFAny //Left argument of the operation
  val R : DFAny //Right argument of the operation
  protected[DFiant] trait __DevRTOp2 extends __DevRTComponent {
    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Naming
    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    override def codeString: String = {
      implicit val refCodeOwner : DSLOwnerConstruct = owner
      s"RTOp2.$typeName(${O.refCodeString}, ${L.refCodeString}, ${R.refCodeString})"
    }
  }
  override private[DFiant] lazy val __dev : __DevRTOp2 = new __DevRTOp2 {}
}
object RTOp2 {
  case class +(O : DFAny.Var, L : DFAny, R : DFAny)(implicit ctx : RTComponent.Context) extends RTOp2
  case class -(O : DFAny.Var, L : DFAny, R : DFAny)(implicit ctx : RTComponent.Context) extends RTOp2
//  def +[L, R](l : DFUInt[Int], r : DFUInt[Int])(implicit ctx : RTComponent.Context) : DFUInt[Int] =
}
