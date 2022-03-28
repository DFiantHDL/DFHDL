import DFiant.*

@dsn class Example:
  val x = DFUInt(8) <> VAR

@main def hello: Unit =
  val top = new Example
  top.printCodeString
