package AES
import dfhdl.*

class Cipher extends DFDesign:
  val key = AESKey <> IN
  val data = AESData <> IN
  val o = AESData <> OUT
  o := cipher(data, key)

@main def main: Unit =
  given options.CompilerOptions.CompilerLogLevel = options.LogLevel.INFO
  Cipher().printCodeString
