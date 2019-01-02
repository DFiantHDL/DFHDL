package RISCV
import DFiant._

case class IMemEntry(addr : BitVector, inst : BitVector, instStr : String)
case class DMemEntry(addr : BitVector, inst : BitVector)

case class ProgramIMem(list : List[IMemEntry], startAddress : BitVector, finishAddress : BitVector, failAddress : Option[BitVector])
case class ProgramDMem(list : List[DMemEntry])

object ProgramIMem {
  import scala.io.Source
  import internals.BitVectorExtras
  private val extractor = """[ \t]*([0-9a-f]+):[ \t]*([0-9a-f]+)[ \t]*(.+)""".r
  private val mainExtractor = """[ \t]*([0-9a-f]+) <main>:[ \t]*""".r
  private val testExtractor = """[ \t]*([0-9a-f]+) <test_2>:[ \t]*""".r
  private val failExtractor = """[ \t]*([0-9a-f]+) <fail>:[ \t]*""".r
  private val passExtractor = """[ \t]*([0-9a-f]+) <pass>:[ \t]*""".r
  private val dataStart = "Disassembly of section .data:"
  def fromFile(progMemFile : String) : ProgramIMem = {
    val file = Source.fromFile(progMemFile)
    var mainAddr : Option[BitVector] = None
    var endAddr : Option[BitVector] = None
    var failAddr : Option[BitVector] = None
    val list = file.getLines.collect {
      case extractor(addr, inst, asm) if mainAddr.isDefined && endAddr.isEmpty && asm == "ret" =>
        endAddr = Some(BitVector.fromHex(addr).get.toLength(32))
        Some(IMemEntry(endAddr.get, BitVector.fromHex(inst).get, asm))
      case extractor(addr, inst, asm) =>
        if (inst.length < 8) None
        else Some(IMemEntry(BitVector.fromHex(addr).get.toLength(32), BitVector.fromHex(inst).get, asm))
      case mainExtractor(addr) =>
        mainAddr = Some(BitVector.fromHex(addr).get.toLength(32))
        None
      case testExtractor(addr) =>
        mainAddr = Some(BitVector.fromHex(addr).get.toLength(32))
        None
      case passExtractor(addr) =>
        endAddr = Some(BitVector.fromHex(addr).get.toLength(32))
        None
      case failExtractor(addr) =>
        failAddr = Some(BitVector.fromHex(addr).get.toLength(32))
        None
      case l if l == dataStart =>
        None
    }.toList.flatten

    file.close
    ProgramIMem(list, mainAddr.getOrElse(list.head.addr), endAddr.getOrElse(list.last.addr), failAddr)
  }
  def empty() : ProgramIMem =
    ProgramIMem(List(), BitVector.fromHex("0").get.toLength(32), BitVector.fromHex("0").get.toLength(32), None)
}


