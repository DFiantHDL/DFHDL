package dfhdl.compiler.printing

extension (text: String)
  private def hasBrackets: Boolean = text.startsWith("(") && text.endsWith(")")

  private def requiresBrackets: Boolean =
    var count: Int = 0
    for (i <- 0 until text.length)
      text.charAt(i) match
        case '(' => count += 1
        case ')' => count -= 1
        case ' ' => if (count == 0) return true
        case _   =>
    text.startsWith("!") || text.startsWith("~") || text.startsWith("-")

  def applyBrackets(onlyIfRequired: Boolean = true): String =
    if (text.requiresBrackets || (!onlyIfRequired && !text.hasBrackets))
      if (text.contains("\n")) s"(\n${text.indent}\n)"
      else s"($text)"
    else text

  def indent: String = indent(1)
  def indent(count: Int): String =
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
end extension
