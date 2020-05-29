package DFiant

@df class TT extends DFDesign {
  val o = DFUInt(8) <> OUT init 0

  import dfsm._
    val st1 = step {
      o := 0
    }
    val st2 = waitUntil(true)

    val st3 = step {
      o := 1
    }
    val st4 = waitUntil(false)
    val st5 = waitForever()

  private val o_addr_fsm = st1 ==> st2 ==> st3 ==> st4 ==> st5
  o_addr_fsm.elaborate
}

object dfsmTest extends App {
  val tt = new TT
  tt.printCodeString()
}
