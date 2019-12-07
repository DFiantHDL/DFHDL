package ZFiant

object ZTest extends App {
  class AAA()(implicit ctx : ContextOf[AAA]) extends DFDesign {

  }

  class BBB(implicit ctx : ContextOf[BBB]) extends AAA {

  }

  abstract class CCC()(implicit ctx : ContextOf[CCC]) extends DFDesign {
    val b = new BBB
    val b2 = new BBB {}
  }


  trait Top extends DFDesign {
    val a = new AAA() {}

    val a2 = new AAA()

    val c = new CCC() {}
  }

  val top = new Top {}

  println((top, top.ownerOption), (top.a, top.a.ownerOption), (top.a2, top.a2.ownerOption), (top.c, top.c.ownerOption))
  println((top.c.b, top.c.b.ownerOption), (top.c.b2, top.c.b2.ownerOption))
//  trait BB extends DFDesign {
//    //    val a = DFUInt(8)
//    //    DFUInt(8).ifdf(???) {
//    //      a
//    //    }.elsedf {
//    //      a
//    //    }
//    val b = DFBits(8) <> OUT init b1s
//    val b2 = DFBits(8) <> IN
//    val C = DFBits(8) <> IN init (b"11111111", b0s)
//    b := b2
//    val bb = b"1111111"
//    val z = b == b1s
//  }

}

