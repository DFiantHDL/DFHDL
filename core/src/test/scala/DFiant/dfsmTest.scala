package DFiant

@df class TT extends DFDesign {
  val o = DFUInt(8) <> OUT init 0

  import fsm._

  val st0 = step{}
  val d_data_fsm = st0 ==> firstStep

  d_data_fsm.elaborate
}

object dfsmTest extends App {
  val tt = new TT
  tt.printCodeString()
}
