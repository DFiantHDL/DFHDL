val line = "  a4:\t01e08193          \taddi\tgp,ra,30"

val extractor = """[ \t]*([0-9a-f]+):[ \t]*([0-9a-f]+)[ \t]*(.+)""".r

line match {
  case extractor(addr, inst, asm) => println(addr, inst, asm)
  case _ =>
}
