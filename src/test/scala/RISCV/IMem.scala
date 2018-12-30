package RISCV
import DFiant._

class IMem_Bram()(implicit ctx : RTComponent.Context) extends RTComponent {
  final val clka = Clock()
  final val addra = DFBits(12) <> IN
  final val douta = DFBits(32) <> OUT
  //  setInitFunc(S)(LazyBox.Args2(this)(DFUInt.Token.+, getInit(A), getInit(B)))
}

trait IMem_Bram_Sim extends DFDesign {
  final val addra = DFBits(12) <> IN
  final val douta = DFBits(32) <> OUT

  douta := b0s
  List(h"00000013").foreachdf(addra) {
    case x => douta := x
  }
}

class IMem(incomingPC : DFBits[32])(implicit ctx : DFDesign.ContextOf[IMem]) extends DFDesign {
  private val pc      = DFBits[32] <> IN
  private val instRaw = DFBits[32] <> OUT

  if (inSimulation) {
    val bram_sim = new IMem_Bram_Sim {}
    bram_sim.addra <> pc(13, 2)
    bram_sim.douta <> instRaw
  } else {
    val bram = new IMem_Bram()
    bram.addra <> pc(13, 2)
    bram.douta <> instRaw
  }


  final val inst = IMemInst(pc = incomingPC, instRaw = instRaw)

  atOwnerDo {
    pc <> incomingPC
  }
}

case class IMemInst(
  pc      : DFBits[32],
  instRaw : DFBits[32]
)

