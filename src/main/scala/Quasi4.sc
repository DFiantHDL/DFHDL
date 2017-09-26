object MyTypes {

  trait RealType {
  }

  abstract class Container[RT <: RealType] {
    this: RT =>
//    def createAnother : Container[RT] with RT
  }

  trait SomeRealType extends RealType

  trait PartialRealType extends RealType {
    val needed: Int
  }
}

import MyTypes._

object Container {
  import scala.reflect.macros.blackbox.Context
  import scala.language.experimental.macros

  def helper[RT <: RealType : c.WeakTypeTag, CT <: Container[RT] : c.WeakTypeTag](c : Context) : c.Expr[RT] = {
    import c.universe._
    val weakRT = "_root_." + weakTypeOf[RT].toString
    val weakCT = "_root_." + weakTypeOf[CT].typeConstructor.toString
    val list = List(AppliedTypeTree(Ident(TypeName(weakCT)), List(Ident(TypeName(weakRT)))), Ident(TypeName(weakRT)))
//    val list = List(q"$weakCT", q"$weakRT")
//    val genTree = q"""
//                      case class Temp() extends ..$list {}
//                      Temp()
//      """
  val genTree = q"new ..$list {}"
  //    val genTree = q"new Container[$weakRT] with $weakRT {}"
    c.Expr(genTree)
  }
  def apply[RT <: RealType] : Container[RT] with RT = macro helper[RT, Container[RT]]
}

//val ok = {
//  case class Temp() extends Container[SomeRealType] with SomeRealType {
//    def createAnother: Container[SomeRealType] with SomeRealType = this.copy()
//  }
//  Temp()
//}
val ok_quasi = Container[SomeRealType]


def test[RT <: RealType : WeakTypeTag, CT <: Container[RT] : WeakTypeTag] = {
  val weakCT = weakTypeOf[RT]
  showRaw(tq"$weakCT")
}
