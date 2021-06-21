import DFiant.*
import munit.*
import internals.{Inlined, AllowTopLevel}
import compiler.printing.{Printer, DefaultPrinter}
import scala.annotation.internal.sharable

class BlaSpec extends FunSuite, AllowTopLevel:
  class Bar(using DFC) extends DFDesign:
    println(typeName)
  class Foo(using DFC) extends DFDesign
//    val barry = new Bar

  val top = new Foo
