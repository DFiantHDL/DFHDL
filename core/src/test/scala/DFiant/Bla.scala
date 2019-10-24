/*
 *     This file is part of DFiant.
 *
 *     DFiant is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU Lesser General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     any later version.
 *
 *     DFiant is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     GNU Lesser General Public License for more details.
 *
 *     You should have received a copy of the GNU Lesser General Public License
 *     along with DFiant.  If not, see <https://www.gnu.org/licenses/>.
 */

package DFiant


trait Bug1 extends DFDesign {
  final val n = 32
  final val a   = DFBits(n) <> IN
  final val res = DFBits(n) <> OUT
  val tempL = DFBits(16)
  val tempR = DFBits(16)
  tempL := a(31, 16)
  tempR := a(15, 0)
  (tempL, tempR).bits.bits(23,16) := h"57"
  res(31, 16) := tempL
  res(15, 0) := tempR
}

//class IMem()(implicit ctx : RTComponent.Context) extends RTComponent {
//  final val clka = Clock()
//  final val addra = DFBits(12) <> IN
//  final val douta = DFBits(32) <> OUT
//}

trait Inst extends DFDesign {
  val g_predict = DFBool() <> OUT
  val l_predict = DFBool() <> OUT
  final val wea = DFBool() <> IN
  g_predict := wea
  l_predict := wea
}


class Cont()(implicit ctx : DFDesign.ContextOf[Cont]) extends DFDesign {
  val i = DFBool() <> IN
  val o = DFUInt(8) <> OUT
  val temp = DFUInt(8) <> IN

//  temp.bits(3,0) := b"1111"
//  temp.bits(7,4) := b"0000"

//  ifdf (i) {
//    temp.bits(7,4) := b"1111"
//    o := temp
//  }
  o <> temp
//  o := temp
//  o := temp
//  temp := temp.prev(5)
//  temp.bits(3,0) := b"1111"
//  temp.bits(7,4) := b"0110"
//  o := temp
}

trait Simy extends DFSimulator {
//  val i = DFBool()
//  val o = DFUInt(8)
  val cont = new Cont()
//  cont.i <> i
//  cont.o <> o
}


trait IODesignConn1 extends DFDesign {
  val i = DFUInt(8) <> IN init(1,2)
  val o = DFUInt(8) <> OUT
  o <> i
}

object Foo extends Enum.Auto {
  val Baz0, Baz1, Baz2, Baz3, Baz4 = Entry
}

trait IODesignMatch extends DFDesign {
  for (i <- 1 to 2) {
    val i1 = DFUInt(8) <> IN init (1, 1, Bubble, 1)
    val i2 = DFUInt(8) <> IN init (2, 8, 7, 11, 21)
    val o1 = DFUInt(8) <> OUT
    val myMatch = matchdf (i2, MatchConfig.AllowOverlappingCases)
      .casedf(1 to 5, 10 to 20) {o1 := i1}
      .casedf(7){o1 := i2}
      .casedf(11){o1 := i2}
      .casedf_{o1 := i2}

  }

  val i1 = DFUInt(8) <> IN init (1, 1, Bubble, 1)
  val i2 = DFUInt(8) <> IN init (2, 8, 7, 11, 21)

  val temp = DFUInt[8]
  temp := temp + 1
  val o2 = DFUInt(8) <> OUT
  val ret = DFUInt(8).matchdf(i2)
    .casedf(1 to 5, 10 to 20) {i1}
    .casedf(7){75}
    .casedf_{88}
  o2 <> ret
  val o3 = DFUInt(8) <> OUT
  temp := 0
  o3 := temp
//
//  val i3 = DFEnum(Foo) <> IN init (Foo.Baz0, Foo.Baz3)
//  val o3 = DFUInt(8) <> OUT
//  val myEnumMatch = matchdf (i3)
//    .casedf(Foo.Baz0) {o3 := 1}
//    .casedf(Foo.Baz1) {o3 := 0}
}

class RTx2(width : Int)(implicit ctx : RTComponent.Context) extends RTComponent {
  final val I = DFUInt(width) <> IN
  final val O = DFUInt(width) <> OUT
  final override protected val blackBoxFunctions = Map(O -> BlackBoxFunction(O)(I, I)((l, r) => l + r))
}

trait Comp extends DFComponent[Comp] {
  val i = DFUInt(8) <> IN
  val o = DFUInt(8) <> OUT
  final override protected val blackBoxFunctions = Map(o -> BlackBoxFunction(o)(i, i)((l, r) => l + r))
}
object Comp {
  implicit val ev : Comp => Unit = ifc => {
    import ifc._
    val rt = new RTx2(8)
    rt.I <> i
    rt.O <> o
  }
}

trait IODesignConn2 extends DFDesign{
  val i = DFUInt(8) <> IN init 1
  val o = DFUInt(8) <> OUT

  val io = new Comp {}
  i <> io.i
  o <> io.o
}

object Bla extends DFApp {
  implicit val config = DFAnyConfiguration.detailed
  val bla = new IODesignConn2 {}
  bla.io.unfold
  bla.printCodeString
  import internals._
//  println(bla.members.map(m => (m.meta, m.nameFirst)).mkString("\n"))
//  println(bla.o.connectionLoop)
//  println(bla.members.collect{case m : ConditionalBlock[_,_] => m.netsTo})
}
