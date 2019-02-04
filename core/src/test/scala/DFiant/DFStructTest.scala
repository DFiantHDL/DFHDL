//package DFiant
//
//import DFiant.TestUtils._
//import org.scalacheck._
//import shapeless.test.illTyped
//import singleton.ops._
//
//class DFStructAutoTest extends Properties("DFStructAutoTest") {
//  trait Foo extends DFFields {
//    val u = DFUInt(8)
//    val b = DFBits(8)
//  }
//
//
//  trait MyDesign extends DFDesign {
//    val p1 = DFStruct(new Foo{}) <> IN
//    val p1dup = DFStruct(Foo) <> OUT
//    p1dup := p1
//
//    val p2 = DFStruct(Foo) <> IN init Foo.Baz2
//    val p3 = DFStruct(Foo) <> OUT
//    p3 := DFStructFromEntry(Foo.Baz1)
//    val e = DFStruct(Foo)
//    val p4 = DFStruct(Foo) <> IN
//    val p5 = DFStruct(Foo) <> OUT
//    e := Foo.Baz1
//    e == Foo.Baz2
//    e.bits(1,0)
//    illRun {e.bits(4,0)}
//  }
//
//  property("Foo.codeString") = {
//    val compare =
//      """
//        |object Foo extends Enum.Auto {
//        |  val Baz0            = Entry()  //0
//        |  val Baz1            = Entry()  //1
//        |  val Baz2            = Entry()  //2
//        |  val Baz3            = Entry()  //3
//        |  val Baz4            = Entry()  //4
//        |}
//      """.stripMargin
//    Foo.codeString =@= compare
//  }
//}
