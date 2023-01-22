package dfhdl.compiler.printing

extension (text: String)
  private def hasBrackets: Boolean = text.startsWith("(") && text.endsWith(")")

  private def requiresBrackets: Boolean =
    var count: Int = 0
    var spaceBracket = false
    for (i <- 0 until text.length; if !spaceBracket)
      text.charAt(i) match
        case '(' => count += 1
        case ')' => count -= 1
        case ' ' => if (count == 0) spaceBracket = true
        case _   =>
    if (spaceBracket) true
    else text.startsWith("!") || text.startsWith("~") || text.startsWith("-")

  def applyBrackets(onlyIfRequired: Boolean = true): String =
    if (text.requiresBrackets || (!onlyIfRequired && !text.hasBrackets))
      if (text.contains("\n")) s"(\n${text.hindent}\n)"
      else s"($text)"
    else text

  //TODO: this used to be called `indent`, but java 12 introduced its own indent and broke things
  //See: https://github.com/lampepfl/dotty/issues/16743
  def hindent: String = hindent(1)
  def hindent(count: Int): String =
    text.linesIterator
      .map(l =>
        if (l.isEmpty) ""
        else "  " * count + l
      )
      .mkString("\n")
  def align(lhsRegx: String, opRegx: String, rhsRegx: String): String =
    val pat = s"($lhsRegx)($opRegx)($rhsRegx)".r
    val maxAlign = text.linesIterator.map {
      case pat(lhs, _, _) => lhs.length
      case _              => 0
    }.max
    if (maxAlign > 0)
      text.linesIterator
        .map {
          case pat(lhs, op, rhs) =>
            val delta = " " * (maxAlign - lhs.length)
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
end extension
