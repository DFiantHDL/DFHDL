package dfhdl.compiler.printing

extension (text: String)
  private def hasBrackets: Boolean = text.startsWith("(") && text.endsWith(")")

  private def requiresBrackets: Boolean =
    var countBrackets: Int = 0
    var countBraces: Int = 0
    var spaceBracket = false
    for (i <- 0 until text.length; if !spaceBracket)
      text.charAt(i) match
        case '(' => countBrackets += 1
        case ')' => countBrackets -= 1
        case '{' => countBraces += 1
        case '}' => countBraces -= 1
        case ' ' => if (countBrackets == 0 && countBraces == 0) spaceBracket = true
        case _   =>
    if (spaceBracket) true
    else text.startsWith("!") || text.startsWith("~") || text.startsWith("-")
  end requiresBrackets

  def applyBrackets(onlyIfRequired: Boolean = true): String =
    if (text.requiresBrackets || (!onlyIfRequired && !text.hasBrackets))
      if (text.contains("\n")) s"(\n${text.hindent}\n)"
      else s"($text)"
    else text

  def betterLinesIterator: Iterator[String] =
    if (text.endsWith("\n"))
      text.linesIterator ++ List("")
    else
      text.linesIterator
  // TODO: this used to be called `indent`, but java 12 introduced its own indent and broke things
  // See: https://github.com/lampepfl/dotty/issues/16743
  def hindent: String = hindent(1)
  def hindent(count: Int): String =
    text.betterLinesIterator
      .map(l =>
        if (l.isEmpty) ""
        else "  " * count + l
      )
      .mkString("\n")
  def align(
      lhsRegx: String,
      opRegx: String,
      rhsRegx: String,
      lhsFilter: String => Boolean = _ => true
  ): String =
    val pat = s"($lhsRegx)($opRegx)($rhsRegx)".r
    var maxAlign = 0
    var alignLengths = List.empty[Int]
    text.betterLinesIterator.foreach {
      case pat(lhs, _, _) =>
        val len = lhs.length()
        alignLengths = len :: alignLengths
        maxAlign = maxAlign max len
      case _ =>
    }

    def maxAfterRemovingOutliers(data: List[Int]): Int =
      // Step 1: Sort the data
      val sortedData = data.sorted

      // Step 2: Calculate Q1 and Q3
      val q1 = quantile(sortedData, 0.25)
      val q3 = quantile(sortedData, 0.75)

      // Step 3: Calculate IQR
      val iqr = q3 - q1

      // Step 4: Determine outlier boundary
      val upperBound = q3 + 1.5 * iqr + 3 // 3 spaces offset added

      // Step 5: Filter the data
      val filteredData = sortedData.filter(_ <= upperBound)

      // Step 6: Return the maximum value of the filtered data
      filteredData match
        case Nil => sortedData.last
        case _   => filteredData.last
    end maxAfterRemovingOutliers

    def quantile(data: List[Int], quantile: Double): Double =
      val sortedData = data.sorted
      val n = sortedData.length
      val index = quantile * (n - 1)
      val lowerIndex = math.floor(index).toInt
      val upperIndex = math.ceil(index).toInt
      if (lowerIndex == upperIndex)
        sortedData(lowerIndex)
      else {
        val lowerValue = sortedData(lowerIndex)
        val upperValue = sortedData(upperIndex)
        lowerValue + (upperValue - lowerValue) * (index - lowerIndex)
      }

    if (alignLengths.length > 0)
      // we remove long align outliers that pass the threshold
      val setAlign = maxAfterRemovingOutliers(alignLengths)
      text.betterLinesIterator
        .map {
          case pat(lhs, op, rhs) if lhsFilter(lhs) =>
            val delta = " " * (setAlign - lhs.length)
            s"$lhs$delta$op$rhs"
          case l => l
        }
        .mkString("\n")
    else text
    end if
  end align
  def colorWords(wordSet: Set[String], color: String): String =
    wordSet.foldLeft(text) { case (t, w) =>
      t.replaceAll(s"\\b$w\\b", s"$color$w${io.AnsiColor.RESET}")
    }
  def colorOps(wordSet: Set[String], color: String): String =
    wordSet.foldLeft(text) { case (t, w) =>
      t.replaceAll(s" $w ", s"$color $w ${io.AnsiColor.RESET}")
    }
  def decolor: String = text.replaceAll("\u001B\\[[;\\d]*m", "")

end extension
