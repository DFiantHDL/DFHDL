package DFiant


object dfsmTest {
  trait TT extends DFDesign {
    val s1 = dfsm.step {}
    val s2 = dfsm.step {}
    val s3 = dfsm.step {}

    val c : DFBool = ???

    val myfsm = s1 ==> s2 ==> s3 =?> true ==> s1 ++ s3 ==> s1 ==> s2
  }
}
