package AES
import dfhdl.*

class Cypher extends DFDesign:
  val key = AESKey <> IN
  val data = AESData <> IN
  val o = AESData <> OUT
  o := cipher(data, key)

@main def main: Unit =
  Cypher().printCodeString
    // top.printCodeString
