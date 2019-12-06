package ZFiant

object ZTest {
  trait BB extends DFBlock {
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
  //  val aa = a.bits.as(DFUInt(8)).bits

  //  a := a
  //  implicitly[aa.Var =:= true]
}

