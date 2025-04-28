package dfhdl.compiler.ir
import scala.io.Source
import scala.util.matching.Regex
import java.io.FileNotFoundException
import scala.util.control.Exception._
import dfhdl.internals.CommonOps.bitsWidth
import dfhdl.internals.*

enum InitFileFormat derives CanEqual:
  case Auto, VerilogBin, VerilogHex
  // AMDXilinxCOE, IntelAlteraMIF, IntelAlteraHEX
  // LatticeMEM, AMDXilinxMEM

enum InitFileUndefinedValue derives CanEqual:
  case Bubbles, Zeros

object InitFileFormat:
  import InitFileFormat.*
  def readInitFile(
      fileName: String,
      fileFormat: InitFileFormat,
      arrLen: Int,
      dataWidth: Int,
      undefinedValue: InitFileUndefinedValue
  ): Vector[(BitVector, BitVector)] =
    val source =
      try Source.fromResource(fileName)
      catch
        case _: FileNotFoundException =>
          try Source.fromFile(fileName)
          catch
            case _: FileNotFoundException =>
              throw new IllegalArgumentException(
                s"Init file not found: $fileName\nmake sure either to place the file in your Scala project resource folder or provide a proper relative/absolute path."
              )

    val fileContents = source.getLines().mkString("\n")
    val detectedFormat = fileFormat match
      case Auto => detectAutoFormat(fileName, fileContents, dataWidth)
      case _    => fileFormat
    try
      (detectedFormat: @unchecked) match
        case VerilogBin => readVerilogBin(fileContents, arrLen, dataWidth, undefinedValue)
        case VerilogHex => readVerilogHex(fileContents, arrLen, dataWidth, undefinedValue)
        // case AMDXilinxCOE   => readAMDXilinxCOE(fileContents, arrLen, dataWidth)
        // case IntelAlteraMIF => readIntelAlteraMIF(fileContents, arrLen, dataWidth)
        // case IntelAlteraHEX => readIntelAlteraHEX(fileContents, arrLen, dataWidth)
        // case AMDXilinxMEM   => readAMDXilinxMEM(fileContents, arrLen, dataWidth)
        // case LatticeMEM     => readLatticeMEM(fileContents, arrLen, dataWidth)
    catch
      case e: DataError =>
        import e.*
        throw new IllegalArgumentException(
          s"Init file error detected in $detectedFormat formatted ${fileName}:$lineNum\n$msg"
        )
    end try
  end readInitFile

  private val verilogCommentPattern = """//.*|/\*.*?\*/""".r
  private val validBinPattern =
    "[01]+".r // currently not supporting [xXzZ_] characters for simplification
  private val validHexPattern =
    "[0-9a-fA-F]+".r // currently not supporting [xXzZ_] characters for simplification
  class DataError(val msg: String, val lineNum: Int) extends IllegalArgumentException(msg)
  extension (content: String)
    private def contentCleanup(
        singleLineCommentPattern: String = "",
        multiLineCommentPattern: String = ""
    ): String =
      val replaceWithNewlines = (comment: String) =>
        val lineCount = comment.count(_ == '\n') // Count the number of newlines in the comment
        "\n" * lineCount // Replace with the same number of newline characters
      // first, remove multiline comments
      val noMultiLineComments =
        if (multiLineCommentPattern.isEmpty) content
        else multiLineCommentPattern.r.replaceAllIn(content, m => replaceWithNewlines(m.matched))
      // second, remove singleline comments
      val noSingleLineComment =
        if (singleLineCommentPattern.isEmpty) noMultiLineComments
        else singleLineCommentPattern.r.replaceAllIn(noMultiLineComments, " ")
      // third, cleanup removing multi-spaces
      val noMultiSpaces = noSingleLineComment.replaceAll("""[ \t]+""", " ").trim
      // finally, trim all lines
      noMultiSpaces.linesIterator.map(_.trim).mkString("\n")
    end contentCleanup
    private def verilogCleanup: String = contentCleanup(
      singleLineCommentPattern = """//.*(\n|\r|\r\n)""",
      multiLineCommentPattern = """/\*[\s\S]*?\*/"""
    )
  end extension

  extension (word: String)
    private def isDataBin: Boolean = validBinPattern.matches(word)
    private def isDataHex: Boolean = validHexPattern.matches(word)

  private def detectAutoFormat(
      fileName: String,
      fileContents: String,
      dataWidth: Int
  ): InitFileFormat =
    val suffix = fileName.split("\\.").last.toLowerCase()
    suffix match
      // case "coe" => AMDXilinxCOE
      // case "mif" => IntelAlteraMIF
      // case "mem" =>
      //   if ("""\#Format\s+=""".r.matches(fileContents)) LatticeMEM
      //   else AMDXilinxMEM
      // case "hex" =>
      //   if (fileContents.startsWith(":")) IntelAlteraHEX
      //   else VerilogHex
      case _ =>
        val firstData =
          fileContents.verilogCleanup.linesIterator
            .filter(line => !line.startsWith("@") && line.nonEmpty)
            .nextOption().getOrElse("").split(" ").headOption.getOrElse("")
        if (firstData.isDataBin && firstData.length() == dataWidth) VerilogBin
        else if (firstData.isDataHex) VerilogHex
        else
          throw new IllegalArgumentException(
            s"Could not automatically detect the init file format of $fileName"
          )
    end match

  end detectAutoFormat

  private def readVerilogStdFile(
      contents: String,
      arrLen: Int,
      dataWidth: Int,
      undefinedValue: InitFileUndefinedValue,
      isBinary: Boolean
  ): Vector[(BitVector, BitVector)] =
    val undefinedCell = undefinedValue match
      case InitFileUndefinedValue.Bubbles => (BitVector.low(dataWidth), BitVector.high(dataWidth))
      case InitFileUndefinedValue.Zeros   => (BitVector.low(dataWidth), BitVector.low(dataWidth))
    val result = Array.fill(arrLen)(undefinedCell)
    // starting at address zero
    // in the verilog standard the address refers to the array index
    var currentAddress = 0
    contents.verilogCleanup.linesIterator.zipWithIndex.forall {
      case (line, lineNum) if currentAddress < arrLen =>
        if (line.nonEmpty)
          line.split(" ").foreach { word =>
            def invalidDataCharacterError() =
              throw new DataError(s"Invalid data character detected: $word", lineNum)
            def invalidDataWidthError(wordWidth: Int) = throw new DataError(
              s"Invalid data width detected (expected $dataWidth bits but found $wordWidth bits): $word",
              lineNum
            )
            // address
            if (word.startsWith("@"))
              val addressStr = word.drop(1)
              catching(classOf[NumberFormatException]).opt(Integer.parseInt(addressStr, 16)) match
                case Some(addr) if addr < arrLen => currentAddress = addr
                case _ => throw new DataError(s"Invalid address specification: $word", lineNum)
            // binary data
            else if (isBinary)
              if (!word.isDataBin) invalidDataCharacterError()
              val dfhdlWord = word.replaceAll("[zZxX]", "?")
              val wordData = DFBits.dataFromBinString(dfhdlWord).toOption.get
              val wordWidth = wordData._1.lengthOfValue.toInt
              // for binary we require exact character width
              if (dfhdlWord.length != dataWidth) invalidDataWidthError(wordWidth)
              result(currentAddress) = wordData
              currentAddress += 1
            // hexadecimal data
            else
              if (!word.isDataHex) invalidDataCharacterError()
              val dfhdlWord = word.replaceAll("[zZxX]", "?")
              val wordData = DFBits.dataFromHexString(dfhdlWord).toOption.get
              val wordWidth = wordData._1.lengthOfValue.toInt
              // hex words can be resized if they are smaller than the expected data width
              if (wordWidth > dataWidth) invalidDataWidthError(wordWidth)
              result(currentAddress) =
                (wordData._1.resize(dataWidth), wordData._2.resize(dataWidth))
              currentAddress += 1
            end if
          }
        end if
        true
      case _ => false // already reached the end of the array, so stopping loop
    }
    result.toVector
  end readVerilogStdFile

  /* https://docs.amd.com/r/2023.1-English/ug901-vivado-synthesis/Initializing-RAM-Contents */
  /* https://docs.amd.com/r/2023.1-English/ug901-vivado-synthesis/Specifying-RAM-Initial-Contents-in-an-External-Data-File */
  /* https://docs.amd.com/r/2023.1-English/ug901-vivado-synthesis/Initializing-Block-RAM-From-an-External-Data-File-Verilog */
  private def readVerilogBin(
      contents: String,
      arrLen: Int,
      dataWidth: Int,
      undefinedValue: InitFileUndefinedValue
  ): Vector[(BitVector, BitVector)] =
    readVerilogStdFile(contents, arrLen, dataWidth, undefinedValue, isBinary = true)
  private def readVerilogHex(
      contents: String,
      arrLen: Int,
      dataWidth: Int,
      undefinedValue: InitFileUndefinedValue
  ): Vector[(BitVector, BitVector)] =
    readVerilogStdFile(contents, arrLen, dataWidth, undefinedValue, isBinary = false)
  /* https://docs.amd.com/r/en-US/ug896-vivado-ip/COE-File-Syntax */
  private def readAMDXilinxCOE(
      contents: String,
      arrLen: Int,
      dataWidth: Int
  ): Vector[(BitVector, BitVector)] =
    ???
  /* https://docs.amd.com/r/en-US/ug1580-updatemem/Memory-Files */
  private def readAMDXilinxMEM(
      contents: String,
      arrLen: Int,
      dataWidth: Int
  ): Vector[(BitVector, BitVector)] =
    ???
  /* https://www.intel.com/content/www/us/en/programmable/quartushelp/current/index.htm#reference/glossary/def_mif.htm */
  private def readIntelAlteraMIF(
      contents: String,
      arrLen: Int,
      dataWidth: Int
  ): Vector[(BitVector, BitVector)] =
    ???
  /* https://www.intel.com/content/www/us/en/programmable/quartushelp/current/index.htm#reference/glossary/def_hexfile.htm */
  private def readIntelAlteraHEX(
      contents: String,
      arrLen: Int,
      dataWidth: Int
  ): Vector[(BitVector, BitVector)] =
    ???
  /* https://www.gsp.com/cgi-bin/man.cgi?topic=srec_mem */
  private def readLatticeMEM(
      contents: String,
      arrLen: Int,
      dataWidth: Int
  ): Vector[(BitVector, BitVector)] =
    ???
end InitFileFormat
