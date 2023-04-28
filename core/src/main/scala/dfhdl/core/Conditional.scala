package dfhdl.core

class Conditional[T](arg: => T, cond: => Boolean):
  def isActive: Boolean = cond
  def getArg: T = arg

object Conditional:
  object Ops:
    extension [T](arg: => T)
      def @@(cond: => Boolean): Conditional[T] = new Conditional[T](arg, cond)
