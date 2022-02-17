package DFiant.compiler.printing

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
end extension
