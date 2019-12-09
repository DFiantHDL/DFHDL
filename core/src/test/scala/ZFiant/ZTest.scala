package ZFiant

object ZTest extends App {
  abstract class AAA()(implicit ctx : ContextOf[AAA]) extends DFDesign {

    val b = DFBits(8)
    val c = DFBits(8) <> OUT
    DFBits(8).ifdf(true) {
      b
    }.elseifdf(1) {
      b"11100111"
    }.elsedf {
      b
    }

    b0s <> b
    b <> c
    ifdf(true){
      b := b0s
    }

    val sm = DFBits(8).matchdf(b)
      .casedf(b"11111111") {
        b0s
      }
      .casedf_ {
        b1s
      }

    c := sm

    matchdf(b)
      .casedf(b"11111111") {
        c := b0s
      }
      .casedf_ {
        c := b1s
      }

  }

  trait BBB extends AAA {

  }

  abstract class CCC()(implicit ctx : ContextOf[CCC]) extends DFDesign {
    val b = new BBB{}
    val b2 = new BBB {}
  }


  trait Top extends DFDesign {
    val a = new AAA() {}

    val b = new BBB {}

    val c = new CCC() {}
  }

  val top = new Top {}

  println((top, top.owner), (top.a, top.a.owner), (top.b, top.b.owner), (top.c, top.c.owner))
  println((top.c.b, top.c.b.owner), (top.c.b2, top.c.b2.owner))
  trait BB extends DFDesign {
    //    val a = DFUInt(8)
    //    DFUInt(8).ifdf(???) {
    //      a
    //    }.elsedf {
    //      a
    //    }
    val b = DFBits(8) <> OUT init b1s
    val b2 = DFBits(8) <> IN
    val C = DFBits(8) <> IN init (b"11111111", b0s)
    b := b2
    val bb = b"1111111"
    val z = b == b1s
  }

}

