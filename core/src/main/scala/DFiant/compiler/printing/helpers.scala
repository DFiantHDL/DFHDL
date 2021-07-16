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
      s"($text)"
    else text

  def delim(count: Int = 1): String = text.replaceAll("(?m)^", "  " * count);
