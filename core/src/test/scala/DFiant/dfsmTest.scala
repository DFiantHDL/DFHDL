package DFiant

@df class TT extends DFDesign {
  val o = DFUInt(8) <> OUT init 0

  import dfsm._
  val part1 =
    doWhile(o < 15) {
      o := o + 1
    } ==>
    doUntil(o === 21) {
      o := o + 1
    }

  val part2 = step{
    last.goto()
  }

  val last = step {}

//  val myfsm = s1 =?> (o > 1) ==> s2 ==> s3 ++ (s1 ==> s3)
  val myfsm = part1 ==> part2 ++ last
  myfsm.elaborate
}

object dfsmTest extends App {
  val tt = new TT
  tt.printCodeString()
}
