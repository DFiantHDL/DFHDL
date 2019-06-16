/*
 *     This file is part of DFiant.
 *
 *     DFiant is free software: you can redistribute it and/or modify
 *     it under the terms of the Lesser GNU General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     any later version.
 *
 *     DFiant is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     Lesser GNU General Public License for more details.
 *
 *     You should have received a copy of the Lesser GNU General Public License
 *     along with DFiant.  If not, see <https://www.gnu.org/licenses/>.
 */

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
