package DFiant.printer

object formatter {
  import io.AnsiColor._
  final def ALGN(idx : Int)(implicit printConfig: Printer.Config) : String = {
    assert(idx < printConfig.maxAlignments.size)
    s"$$$$${idx}$$$$"
  }
  final val EMPTY = "$$EMPTY$$"
  private val colorCode = "\u001B\\[[;\\d]*m"
  private val optionalSpaces = "[ \t]*"
  private val word = "([`0-9a-zA-Z_]+)"
  private val operator = "([<>+\\-*/=:!^&%|#]+)"
  private val string = """(".*")"""
  private val noreset = "\u001B{0}"
  private val singleChar = "([();{}\\[\\]])"
  private val coloredSymbol = s"($colorCode)$optionalSpaces(($word|$operator|$string|$singleChar){1})$noreset".r.unanchored
  implicit class FormatString(text : String) {
    private[FormatString] def colored : String = coloredSymbol.replaceAllIn(text, m => s"${m.group(1)}${m.group(2)}$RESET")
    def colorWords(wordSet : Set[String], color : String) : String = {
      wordSet.foldLeft(text){case (t, w) => t.replaceAll(s"\\b$w\\b", s"$color$w")}
    }
    def uncolor : String = text.replaceAll(colorCode, "")
    private[FormatString] def explicitEmptyLines : String = text.replaceAll("(?m)^\\s*$[\n\r]{1,}", "").replace(EMPTY, "")
    def removeAlignment(implicit printConfig: Printer.Config) : String = {
      printConfig.maxAlignments.indices.foldLeft(text) {
        case (text, i) => text.replace(ALGN(i), "")
      }
    }
    def formatted(implicit printConfig: Printer.Config) : String = {
      printConfig.maxAlignments.zipWithIndex.foldLeft(text.colored.explicitEmptyLines){case (algnText, (algnMax, algnIdx)) =>
        val uncolored = algnText.uncolor
        val posList : List[Int] = uncolored.linesIterator.map(l => l.indexOf(ALGN(algnIdx))).toList
        val maxPos = posList.max
        posList.filter(_ >= 0).minOption match {
          case Some(minPos) =>
            val maxAddedSpaces = (maxPos - minPos) min algnMax
            val alignPos = minPos + maxAddedSpaces
            val addedSpaceList = posList.map {
              case i if i >= 0 && i < alignPos => (alignPos - i)// min maxAddedSpaces
              case _ => 0
            }
            (algnText.linesIterator zip addedSpaceList).map{case (line, space) => line.replace(ALGN(algnIdx), " "*space)}.mkString("\n")
          case None => algnText
        }
      }
    }
    private[FormatString] def hasBrackets : Boolean = text.startsWith("(") && text.endsWith(")")
    private[FormatString] def requiresBrackets : Boolean = {
      var count : Int = 0
      for (i <- 0 until text.length) {
        text.charAt(i) match {
          case '(' => count += 1
          case ')' => count -= 1
          case ' ' => if (count == 0) return true
          case _ =>
        }
      }
      false
    }
    def applyBrackets(onlyIfRequired : Boolean = true) : String = {
      val uncolored = text.replaceAll(s"($colorCode)$optionalSpaces", "")
      if (uncolored.requiresBrackets || (!onlyIfRequired && !uncolored.hasBrackets)) s"($text)" else text
    }

    def delim(count : Int = 1)(implicit printConfig: Printer.Config) : String = text.replaceAll("(?m)^", printConfig.DELIM * count);
  }
}
