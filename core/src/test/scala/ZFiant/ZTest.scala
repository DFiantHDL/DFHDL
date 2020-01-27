package ZFiant

object ZTest extends App {
  trait ConnTest extends DFDesign {
    val bool = DFBool() <> IN
    val pI = DFBits(8) <> IN
    val pO = DFBits(8) <> OUT
    val b = DFBits(8)
    val or = b | b

    bool <> bool
    bool <> (bool || bool)
    bool || true
    true || bool
    pI <> pI
    pO <> pI
    pI <> pO
    pO <> pO

    pI <> b
    b <> pI
    pO <> b
    b <> pO

    or <> pO
    or <> pI

    b0s <> pO
    pO <> b0s
    pI <> b0s
    b0s <> pO

    //errors
//    1 <> pO
//    pO <> 1
//    b <> b
//    or <> b
//    or <> or
  }
  abstract class AAA()(implicit ctx : ContextOf[AAA]) extends DFDesign {

    val b = DFBits(8) init b0s

    val c = DFBits(8) <> OUT
    DFBits(8).ifdf(true) {
      b
    }.elseifdf(1) {
      b"11100111"
    }.elsedf {
      b
    }

    b"11111111" | b
    val z = b | b
    b | b"11111111"

    b0s <> c
    b <> c
    z <> c
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

  trait BBB extends DFDesign {
    val i = DFBits(8) <> IN init b0s
    val o = DFBits(8) <> OUT setName("haha")

    val b = i == b0s
    val ret = DFBits(8).ifdf(b) {
      i & i | i
    }.elsedf {
      i | i & i.prev
    }

    val ret2  = DFBits(8).matchdf(ret)
        .casedf(b"00000000") {b0s}
        .casedf(b"11110000") {i}
        .casedf_ {i}
    val x = ret2 == b"00000000"
    o <> (ret2 | ret2)

    val u8 = DFUInt(8)
    val u7 = DFUInt(7)
    u8 + u8
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

  trait ID extends DFDesign {
    val i = DFBits(8) <> IN
    val o = DFBits(8) <> IN
    val trying = new Trying {}
    trying.i <> i
    trying.o <> o
  }
  trait Trying extends DFDesign {
    val i = DFBits(8) <> IN
    val o = DFBits(8) <> OUT
    ifdf(i === b0s) {
      o.bits(3,0) := b0s
      o.bits(7,4) := b0s
      o.bits(3,0) := o.bits(7,4)
    }.elsedf {
      o := i
    }
//    o := o
  }

  val top = new BBB {}

  import DFCompiler._
  val trying = new Trying {}

  trying.printCodeString().explicitPrev.printCodeString()

//  top.db.calcInit.printCodeString()(PrintConfig.ShowInits)
//  top.db.patch(Map(top.i -> top.i.setName("bobby"))).printOwnerMemberList()
//  top.db.patch(Map(top.i -> top.i.copy(meta = top.i.meta.copy(top.i.meta.name.copy(value = "HAHA"))))).printOwnerMemberList()


//  println(top.__compiler.getRefTable)

//  println((top, top.owner), (top.a, top.a.owner), (top.b, top.b.owner), (top.c, top.c.owner))
//  println((top.c.b, top.c.b.owner), (top.c.b2, top.c.b2.owner))
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

