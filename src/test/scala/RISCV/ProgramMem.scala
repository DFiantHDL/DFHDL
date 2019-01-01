package RISCV
import DFiant._

case class MemEntry(addr : BitVector, inst : BitVector, instStr : String) {
//  println(this)
}

case class ProgramMem(list : List[MemEntry], startAddress : BitVector)

object ProgramMem {
  import scala.io.Source
  import internals.BitVectorExtras
  private val extractor = """[ \t]*([0-9a-f]+):[ \t]*([0-9a-f]+)[ \t]*(.+)""".r
  private val mainExtractor = """[ \t]*([0-9a-f]+) <main>:[ \t]*""".r
  private val testExtractor = """[ \t]*([0-9a-f]+) <test_2>:[ \t]*""".r

  def fromFile(progMemFile : String) : ProgramMem = {
    val file = Source.fromFile(progMemFile)
    var mainAddr : Option[BitVector] = None
    val list = file.getLines.collect {
      case extractor(addr, inst, asm) =>
        if (inst.length < 8) None
        else Some(MemEntry(BitVector.fromHex(addr).get.toLength(32), BitVector.fromHex(inst).get, asm))
      case mainExtractor(addr) =>
        mainAddr = Some(BitVector.fromHex(addr).get.toLength(32))
        None
      case testExtractor(addr) =>
        mainAddr = Some(BitVector.fromHex(addr).get.toLength(32))
        None
    }.toList.flatten

    file.close
    ProgramMem(list, mainAddr.getOrElse(list.head.addr))
  }
  def empty() : ProgramMem = ProgramMem(List(), BitVector.fromHex("0").get.toLength(32))
}


