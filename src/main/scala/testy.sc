import DFiant._

import DFComponent.Implementation

implicit object basicLib extends DFiant.basiclib.DFBasicLib {
  implicit def `ev+`[LW, RW, WCW] : Implementation[`U+U`[LW, RW, WCW]] = ifc => {
    import ifc._
  }
  implicit def `ev-`[LW, RW, WCW] : Implementation[`U-U`[LW, RW, WCW]] = ifc => {
    import ifc._
  }
  implicit def `ev*`[LW, RW, WCW] : Implementation[`U*U`[LW, RW, WCW]] = ifc => {
    import ifc._
  }

  implicit def `ev==`[LW, RW] : Implementation[`U==U`[LW, RW]] = ifc => {
    import ifc._
  }
  implicit def `ev!=`[LW, RW] : Implementation[`U!=U`[LW, RW]] = ifc => {
    import ifc._
  }
  implicit def `ev<`[LW, RW] : Implementation[`U<U`[LW, RW]] = ifc => {
    import ifc._
  }
  implicit def `ev>`[LW, RW] : Implementation[`U>U`[LW, RW]] = ifc => {
    import ifc._
  }
  implicit def `ev<=`[LW, RW] : Implementation[`U<=U`[LW, RW]] = ifc => {
    import ifc._
  }
  implicit def `ev>=`[LW, RW] : Implementation[`U>=U`[LW, RW]] = ifc => {
    import ifc._
  }
}


trait Box extends DFDesign {
  val in1 : DFUInt[Int] <> IN = 0
  val in2 : DFUInt[Int] <> IN = OPEN
  val out1 : DFUInt[Int] <> OUT = OPEN
}

val box = new Box{}

box.portsIn
//class Foo(i : String) {
//  override def toString: String = i
//}
//
//trait Box {
//  val in : Foo
//  val out : Foo
//
//  lazy val ports : Array[Foo] = {
//    getClass.getDeclaredFields.filter(f => f.getType.isAssignableFrom(classOf[Foo]))
//      .map(f => {
//      f.setAccessible(true)
//      f.get(this).asInstanceOf[Foo]
//    })
//  }
//}
//
//trait Top {
//  val f1 = new Foo("1111")
//  val f2 = new Foo("2222")
//  val box = new Box {
//    val in = f1
//    val out = f2
//  }
//  println(box.ports(1))
//}
//
//
//new Top {}