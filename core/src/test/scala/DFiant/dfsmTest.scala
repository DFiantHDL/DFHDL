package DFiant

@df class TT extends DFDesign {
  val o = DFUInt(8) <> OUT
  val myfsm = dfsm.step {} ==> dfsm.step {} ==> dfsm.step {}

//  val myfsm = s1 =?> (o > 1) ==> s2 ==> s3 ++ (s1 ==> s3)

  myfsm.elaborate
}

object dfsmTest extends App {
  val tt = new TT
  tt.printCodeString()
}
