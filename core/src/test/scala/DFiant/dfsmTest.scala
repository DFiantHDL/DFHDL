package DFiant

@df class TT extends DFDesign {
  val o = DFUInt(8) <> OUT
  val s1 = dfsm.step {}
  val s2 = dfsm.step {}
  val s3 = dfsm.step {}

  val myfsm = s1 ==> s2 ==> s3
  myfsm.elaborate
}

object dfsmTest extends App {
  val tt = new TT
  tt.printCodeString()
}
