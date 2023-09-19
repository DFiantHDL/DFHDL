package AES
import dfhdl.*

class Cypher extends DFDesign:
  val data = AESWord <> IN
  data.subWord

@main def main: Unit =
  Cypher().printCodeString
    // top.printCodeString
