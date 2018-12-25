package RISCV
import DFiant._

class IMem_Bram()(implicit ctx : RTComponent.Context) extends RTComponent {
  final val clka = Clock()
  final val addra = DFBits(12) <> IN
  final val douta = DFBits(32) <> OUT
  //  setInitFunc(S)(LazyBox.Args2(this)(DFUInt.Token.+, getInit(A), getInit(B)))
}

trait IMem extends DFDesign {
  private val pc      = DFBits[32] <> IN
  private val instRaw = DFBits[32] <> OUT

  val bram = new IMem_Bram()
  bram.addra <> pc(13, 2)
  bram.douta <> instRaw

  val inst = IMemInst(pc = pc, instRaw = instRaw)
}

object IMem {
  def apply(pc : DFBits[32])(implicit ctx : DFDesign.Context) : IMem = {
    val imem = new IMem {}.setName("imem")
    imem.pc <> pc
    imem
  }
}

case class IMemInst(
  pc : DFBits[32],
  instRaw : DFBits[32]
)

