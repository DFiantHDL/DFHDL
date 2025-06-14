package CoreSpec
import dfhdl.*
import munit.*
import internals.*

import scala.annotation.{Annotation, nowarn, targetName}
import scala.collection.immutable.ListMap

class PluginSpec extends DFSpec:
  var nameStack: List[Option[String]] = Nil
  var posStack: List[Position] = Nil
  def assertLastNames(names: String*): Unit =
    assertEquals(
      nameStack,
      names.view.reverse.map(n => if (n.isEmpty) None else Some(n)).toList
    )
    nameStack = Nil
  def clearNameStack(): Unit = nameStack = Nil
  def clearPosStack(): Unit = posStack = Nil
  def getLastNames: List[Option[String]] =
    val ret = nameStack
    nameStack = Nil
    ret
  def getLastPos: List[Position] =
    val ret = posStack
    posStack = Nil
    ret

  val fileName = new Throwable().getStackTrace().head.getFileName
  class Bar(using val ctx: DFC) extends OnCreateEvents:
    type This <: Bar
    val nameOpt = ctx.nameOpt
    val pos = ctx.position
    assert(pos.file.endsWith(fileName))
    @inline def +(that: Bar)(using DFC): Bar = new Plus(this, that)
    inline def -(that: Bar)(using DFC): Bar = new Plus(this, that)
    @inline def /[T](that: Exact[T])(using DFC): Bar = new Plus(this, that.value.asInstanceOf[Bar])

    final override def onCreateEnd(thisOwner: Option[This]): Unit =
      nameStack = ctx.nameOpt :: nameStack
      posStack = ctx.position :: posStack

  class Plus(lhs: Bar, rhs: Bar)(using DFC) extends Bar

  val pls3 = new Bar + new Bar + new Bar
  assertLastNames("", "", "", "", "pls3")
  val pls3Pos = getLastPos
  val min3 = new Bar - new Bar - new Bar
  assertLastNames("", "", "", "", "min3")
  val min3Names = getLastNames
  val min3Pos = getLastPos
  assertEquals(
    pls3Pos,
    min3Pos.map(
      _.copy(
        lineStart = pls3Pos.head.lineStart,
        lineEnd = pls3Pos.head.lineStart
      )
    )
  )
  val div3 = new Bar / new Bar / new Bar
  assertLastNames("", "", "", "", "div3")
  val div3Names = getLastNames
  val div3Pos = getLastPos
  assertEquals(
    pls3Pos,
    div3Pos.map(
      _.copy(
        lineStart = pls3Pos.head.lineStart,
        lineEnd = pls3Pos.head.lineStart
      )
    )
  )
  val bar_vec1 = for i <- 0 until 4 yield new Bar
  assertLastNames("bar_vec1", "bar_vec1", "bar_vec1", "bar_vec1")

  val bar_vec2 = Vector.fill(3)(new Bar)
  assertLastNames("bar_vec2", "bar_vec2", "bar_vec2")

  val (barTuple1, barTuple2, barTuple3) = (new Bar, new Bar, new Bar)
  assertLastNames("barTuple1", "barTuple2", "barTuple3")

  inline def wrapper(inline block: DFC ?=> Bar)(using dfc: DFC): Bar =
    block(using dfc)

  val wrappedName = wrapper {
    new Bar
  }
  assertLastNames("wrappedName")

  @metaContextForward(0)
  def forwardContext(block: => Bar)(using DFC) = block
  val fb = forwardContext(new Bar)
  assert(fb.nameOpt == Some("fb"))
  clearNameStack()

  val tryName =
    try new Bar
    catch case _ => ???
  assertLastNames("tryName")

  inline def wrapperTry(block: DFC ?=> Bar)(using dfc: DFC): Bar =
    try block(using dfc)
    catch case _ => ???

  val wrappedTryName = wrapper {
    new Bar
  }
  assertLastNames("wrappedTryName")

  trait HasNamePosWithVars extends internals.HasClsMetaArgs:
    private var _clsName: String = ""
    private var _clsPosition: Position = Position.unknown
    private var _clsDocOpt: Option[String] = None
    private var _clsAnnotations: List[Annotation] = Nil

    final protected def setClsNamePos(
        name: String,
        position: Position,
        docOpt: Option[String],
        annotations: List[Annotation]
    ): Unit =
      _clsName = name
      _clsPosition = position
      _clsDocOpt = docOpt
      _clsAnnotations = annotations

    final def clsName: String = _clsName
    final def clsPosition: Position = _clsPosition
    final def clsDocOpt: Option[String] = _clsDocOpt
    final def clsAnnotations: List[Annotation] = _clsAnnotations
  end HasNamePosWithVars

  /** This is doc */
  @nowarn
  class GotName(x: Int, y: String, z: Int) extends HasNamePosWithVars
  val gotName = new GotName(1, "2", 3)
  assertEquals(gotName.clsName, "GotName")
  assertEquals(gotName.clsDocOpt, Some(" This is doc "))
  assert(gotName.clsAnnotations.head.isInstanceOf[nowarn])
  extension (bar: Bar)(using DFC) @inline def ++(that: Bar): Bar = new Plus(bar, that)

  extension (bar: Bar) @inline def +++(that: Bar)(using DFC): Bar = new Plus(bar, that)

  extension (bar: Bar)
    @metaContextDelegate
    def <>(that: Int): Bar = ???
    @metaContextDelegate
    def setName(name: String): Bar = ???

  class Foo(arg1: Int, arg2: Int)(using DFC) extends Bar

  object Internal:
    class Foo[T](arg1: Int, arg2: Int)(using DFC) extends Bar

  @inline def newBar(using DFC): Bar = new Bar

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
    assertLastNames("internalFoo", "NB1", "nb2")
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

  val top = new Top
  assertLastNames("top")
end PluginSpec
