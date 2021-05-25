package DFiant.compiler.printing

trait NCCode:
  def codeString(using Printer): String

trait WCCode extends NCCode:
  def codeString(using CPrinter): String
