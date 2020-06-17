package DFiant

@df class TT extends DFDesign {
  val o = DFUInt(8) <> OUT init 0

  import fsm._

}

object dfsmTest extends App {
  val tt = new TT
  tt.printCodeString()
}
