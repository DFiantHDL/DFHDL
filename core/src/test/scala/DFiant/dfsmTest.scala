package DFiant

@df class TT extends DFDesign {
  val o = DFUInt(8) <> OUT init 0
  val myfsm =
    dfsm.doWhile(o < 15) {
      o := o + 1
    } ==>
    dfsm.doUntil(o === 21) {
      o := o + 1
    } ==>
    dfsm.wait

//  val myfsm = s1 =?> (o > 1) ==> s2 ==> s3 ++ (s1 ==> s3)

  myfsm.elaborate
}

object dfsmTest extends App {
  val tt = new TT
  tt.printCodeString()
}
