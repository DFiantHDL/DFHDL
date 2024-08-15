package AES
import dfhdl.*

@top(false) class Cipher extends DFDesign:
  val key = AESKey <> IN
  val data = AESData <> IN
  val o = AESData <> OUT
  o := cipher(data, key)
