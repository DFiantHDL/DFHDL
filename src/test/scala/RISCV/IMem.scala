package RISCV
import DFiant._
import internals.BitVectorExtras

class IMem_Bram(programMem : ProgramMem)(implicit ctx : RTComponent.Context) extends RTComponent {
  final val clka = Clock()
  final val addra = DFBits[12] <> IN
  final val douta = DFBits[32] <> OUT
  //  setInitFunc(S)(LazyBox.Args2(this)(DFUInt.Token.+, getInit(A), getInit(B)))
  //need to generate COE file
}

class IMem_Bram_Sim(programMem : ProgramMem)(implicit ctx : DFDesign.ContextOf[IMem_Bram_Sim]) extends DFDesign {
  final val addra = DFBits[12] <> IN
  final val douta = DFBits[32] <> OUT

  douta := b0s
  programMem.list.map(e => (e.addr.bits(13, 2), e.inst)).matchdf[12,32](addra, douta)
}

class IMem(programMem : ProgramMem)(incomingPC : DFBits[32])(implicit ctx : DFDesign.ContextOf[IMem]) extends DFDesign {
  private val pc      = DFBits[32] <> IN
  private val instRaw = DFBits[32] <> OUT

  if (inSimulation) {
    val bram_sim = new IMem_Bram_Sim(programMem)
    bram_sim.addra <> pc(13, 2)
    bram_sim.douta <> instRaw
  } else {
    val bram = new IMem_Bram(programMem)
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


