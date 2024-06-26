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
    var cnt = 0
    var sum = 0
    var maxAlign = 0
    text.betterLinesIterator.foreach {
      case pat(lhs, _, _) =>
        cnt = cnt + 1
        val len = lhs.length()
        sum = sum + len
        maxAlign = maxAlign max len
      case _ =>
    }
    if (cnt > 0)
      val avgAlign = sum / cnt
      val setAlign =
        if (cnt > 10) (avgAlign + maxAlign) / 2 + 3
        else maxAlign
      text.betterLinesIterator
        .map {
          case pat(lhs, op, rhs) if lhsFilter(lhs) =>
            val delta = " " * (setAlign - lhs.length)
            s"$lhs$delta$op$rhs"
          case l => l
        }
        .mkString("\n")
    else text
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
