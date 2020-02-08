package ZFiant

package object RISCV {
  implicit object ALUSel extends Enum.Manual(4) {
    val ADD, SUB, SLL, SRL, SRA, AND, OR, XOR, SLT, SLTU, COPY1 = EntryDelta()
    val DontCare = ADD
  }
  type ALUSel = ALUSel.type
}
