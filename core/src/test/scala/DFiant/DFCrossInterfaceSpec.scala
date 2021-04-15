package DFiant

import DFiant.TestUtils._

class DFCrossInterfaceSpec extends DFTopSpec {
  @df class UARTIfc extends DFCrossInterface {
    val tx = DFBit <> OUT
    val rx = DFBit <> IN

    override protected[DFiant] def crossConnection(twin: UARTIfc.this.type)(implicit ctx: DFBlock.Context): Unit = {
      rx <> twin.tx
      tx <> twin.rx
    }
  }

  @df class UART extends DFDesign {
    val io = new UARTIfc
  }

  @df class Top extends DFDesign {
    val uart1 = new UART
    val uart2 = new UART
    uart1.io <> uart2.io
  }

  val top = new Top

  val expectedCodeString : String =
    """|@df final class UART extends DFDesign {
       |  val io_tx   = DFBit <> OUT
       |  val io_rx   = DFBit <> IN
       |}
       |
       |@df final class Top extends DFDesign {
       |  val uart1   = new UART {}
       |  val uart2   = new UART {}
       |  uart1.io_rx <> uart2.io_tx
       |  uart2.io_rx <> uart1.io_tx
       |}""".stripMargin

  test("codeString generation") {
    assert(top.codeString =@= expectedCodeString)
  }
}

