package dfhdl.compiler.printing

trait HasCodeString:
  def codeString(using Printer): String
