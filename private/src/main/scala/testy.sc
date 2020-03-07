import io.AnsiColor._

val p1 = "(\u001B\\[[;\\d]*m)[ ]*([0-9a-zA-Z_]+)\u001B{0}".r.unanchored

//val p = "\\u001B\\\\[[;\\\\d]*m([0-9a-zA-Z_]+)".r
s"$RED prev" match {
  case p1(v1, v2) => println(s"$v1$v2$RESET")
  case _ => println("bad")
}
val text1 =
  s"""$RED prev bla bla $GREEN prev2
     |${RED}prev${RESET} bla bla ${GREEN}prev2""".stripMargin
val text2 = p1.replaceAllIn(text1, m => s"${m.group(1)}${m.group(2)}$RESET")
p1.replaceAllIn(text2, m => s"${m.group(1)}${m.group(2)}$RESET")
