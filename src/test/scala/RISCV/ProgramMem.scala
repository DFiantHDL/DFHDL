package RISCV
import DFiant._

case class MemEntry(addr : BitVector, inst : BitVector, instStr : String) {
  println(addr, inst, instStr)
}

case class ProgramMem(list : List[MemEntry])

object ProgramMem {
  import scala.io.Source
  import internals.BitVectorExtras
  private val extractor = """[ \t]*([0-9a-f]+):[ \t]*([0-9a-f]+)[ \t]*(.+)""".r

  def fromFile(progMemFile : String) : ProgramMem = {
    val file = Source.fromFile(progMemFile)
    val list = file.getLines.collect {
      case extractor(addr, inst, asm) =>
        MemEntry(BitVector.fromHex(addr).get.toLength(32), BitVector.fromHex(inst).get, asm)
    }.toList

    file.close
    ProgramMem(list)
  }
  def empty() : ProgramMem = ProgramMem(List())
}


