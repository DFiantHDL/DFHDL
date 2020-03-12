package ZFiant.compiler.printer

class Formatter(maxAlignments : List[Int]) {
  import io.AnsiColor._
  private[printer] def ALGN(idx : Int) : String = {
    assert(idx < maxAlignments.size)
    s"$$$$${idx}$$$$"
  }
  private val colorCode = "\u001B\\[[;\\d]*m"
  private val optionalSpaces = "[ ]*"
  private val word = "([0-9a-zA-Z_]+)"
  private val operator = "([<>+\\-*/=:!^&%|#]+)"
  private val string = """(".*")"""
  private val noreset = "\u001B{0}"
  private val singleChar = "([();{}\\[\\]])"
  private val coloredSymbol = s"($colorCode)$optionalSpaces(($word|$operator|$string|$singleChar){1})$noreset".r.unanchored
  implicit class ColoringString(text : String) {
    def colored : String = coloredSymbol.replaceAllIn(text, m => s"${m.group(1)}${m.group(2)}$RESET")
    def uncolor : String = text.replaceAll(colorCode, "")
    def aligned : String = {
      maxAlignments.zipWithIndex.foldLeft(text){case (algnText, (algnMax, algnIdx)) =>
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
  }
}
