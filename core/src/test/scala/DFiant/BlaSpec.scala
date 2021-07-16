import DFiant.*
import munit.*
import internals.{Inlined, AllowTopLevel}
import compiler.printing.{Printer, DefaultPrinter}
import scala.annotation.internal.sharable

class BlaSpec extends FunSuite, AllowTopLevel:
  class Foo(using DFC) extends DFDesign:
    val x = DFBits(8) <> IN

  val top = new Foo
  top.printCodeString()
