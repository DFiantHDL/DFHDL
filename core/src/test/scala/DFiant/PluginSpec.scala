import DFiant.*
import munit.*
import internals.*
import scala.annotation.targetName

class PluginSpec extends DFSpec:
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
      ctx.nameOpt
    def +(that: Bar)(using DFC): Bar = new Plus(this, that)

    override def onCreateEnd: Unit =
      nameStack = ctx.nameOpt :: nameStack

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
    object FooObj extends Foo(1, 2):
      new Bar
      val insiderObj = new Bar
      assertLastNames("", "insiderObj")
    assertLastNames("FooObj")
    case object FooCaseObj extends Foo(1, 2)
    assertLastNames("FooCaseObj")

    @targetName("foo")
    val -- = new Foo(1, 2)
    assertLastNames("foo")
    val fooCls2 = new Foo(1, 2):
      val i = 1
      new Bar
      val insider = new Bar
    assertLastNames("", "insider", "fooCls2")
    val internalFoo = new Internal.Foo(1, 2)
    val nb1 = newBar setName "NB1"
    val nb2 = newBar
    assertLastNames("internalFoo", "nb1", "nb2")
    val plus = nb1 + nb2
    assertLastNames("plus")
    val plus3 = nb1 + nb2 + nb1
    assertLastNames("", "plus3")
    val pp = nb1 ++ nb2
    val ppp = nb1 +++ nb2
    assertLastNames("pp", "ppp")
    val barPlus = new Bar + nb1
    assertLastNames("", "barPlus")
    val fooClsBlock =
      new Foo(1, 2)
      val x = 1
      new Foo(11, 12)
    assertLastNames("", "fooClsBlock")

    case class FooCC(arg1: Int, arg2: Int)(using DFC) extends Foo(arg1, arg2)

    val fooCC = FooCC(1, 2)
    val fooNewCC = new FooCC(1, 2)
    assertLastNames("fooCC", "fooNewCC")
  end Top

  //  given ctx: DFC = ???
  //    DFC(Some("top"), Position.unknown, false, None, Position.unknown)

  val top = new Top
  assertLastNames("top")
end PluginSpec
