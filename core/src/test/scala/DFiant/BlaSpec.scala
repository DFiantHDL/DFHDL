import DFiant.*
import munit.*
import internals.{Inlined, AllowTopLevel}
import compiler.printing.{Printer, DefaultPrinter}

class BlaSpec extends FunSuite, AllowTopLevel:
//  class Foo(using DFC) extends DFDesign:
//    val x = DFBits(8) <> IN init Zeros
//    val y = DFBits(8) <> OUT

  val top = new Foo
  top.printCodeString()
