package RISCV
import DFiant._

class IMem_Bram()(implicit ctx : RTComponent.Context) extends RTComponent {
  final val clka = Clock()
  final val addra = DFBits(12) <> IN
  final val douta = DFBits(32) <> OUT
  //  setInitFunc(S)(LazyBox.Args2(this)(DFUInt.Token.+, getInit(A), getInit(B)))
}

trait IMem extends DFDesign {
  private val addr = DFBits[32] <> IN
  private val inst = DFBits[32] <> OUT

  val bram = new IMem_Bram()
  bram.addra <> addr(13, 2)
  bram.douta <> inst

  def readConn(addr : DFBits[32])(implicit ctx : DFDesign.Context) : DFBits[32] = {
    this.addr <> addr
    this.inst
  }
}

