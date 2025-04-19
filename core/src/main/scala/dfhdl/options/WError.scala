package dfhdl.options

enum WError derives CanEqual:
  case Scalac
  case DFHDL(flag: Boolean)
  def toBoolean: Boolean = this match
    case Scalac      => false
    case DFHDL(flag) => flag
  def fromScalac(flag: Boolean): WError = this match
    case Scalac => DFHDL(flag)
    case _      => this

object WError:
  given WError = Scalac
  given Conversion[Boolean, WError] = DFHDL(_)
