import DFiant.*
import munit.*
import internals.*
import scala.annotation.targetName

class PluginSpec extends FunSuite, AllowTopLevel:
  var nameStack: List[Option[String]] = Nil
  def assertLastNames(names: String*): Unit =
    assertEquals(
      nameStack,
      names.view.reverse.map(n => if (n.isEmpty) None else Some(n)).toList
    )
    nameStack = Nil
  def clearNameStack(): Unit = nameStack = Nil

  class Bar(using val ctx: DFC) extends OnCreateEvents, LateConstruction:
    val nameOpt =
      nameStack = ctx.nameOpt :: nameStack
      ctx.nameOpt
    def +(that: Bar)(using DFC): Bar = new Plus(this, that)

  class Plus(lhs: Bar, rhs: Bar)(using DFC) extends Bar

  extension (bar: Bar)(using DFC) def ++(that: Bar): Bar = new Plus(bar, that)

  extension (bar: Bar) def +++(that: Bar)(using DFC): Bar = new Plus(bar, that)

  extension (bar: Bar)
    @metaContextDelegate
    def <>(that: Int): Bar = ???
    @metaContextDelegate
    def setName(name: String): Bar = ???

  class Foo(arg1: Int, arg2: Int)(using DFC) extends Bar

  object Internal:
    class Foo(arg1: Int, arg2: Int)(using DFC) extends Bar

  def newBar(using DFC): Bar = new Bar

  class Top(using DFC) extends Bar:
    assertLastNames("top")
    object FooObj extends Foo(1, 2):
      new Bar
      val insiderObj = new Bar
    FooObj.hashCode()
    assertLastNames("FooObj", "", "insiderObj")
    case object FooCaseObj extends Foo(1, 2)

    @targetName("foo")
    val -- = new Foo(1, 2)
    val fooCls2 = new Foo(1, 2):
      val i = 1
      new Bar
      val insider = new Bar
    val internalFoo = new Internal.Foo(1, 2)
    val nb1 = newBar setName "NB1"
    val nb2 = newBar
    val plus = nb1 + nb2
    val plus3 = nb1 + nb2 + nb1
    val pp = nb1 ++ nb2
    val ppp = nb1 +++ nb2
    val barPlus = new Bar + nb1
    val fooClsBlock =
      new Foo(1, 2)
      println("fooBlock")
      new Foo(11, 12)

    case class FooCC(arg1: Int, arg2: Int)(using DFC) extends Foo(arg1, arg2)

    val fooCC = FooCC(1, 2)
    val fooNewCC = new FooCC(1, 2)
  end Top

  //  given ctx: DFC = ???
  //    DFC(Some("top"), Position.unknown, false, None, Position.unknown)

  val top = new Top

  println(top.nb1.nameOpt)
end PluginSpec
