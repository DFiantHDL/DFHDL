package DFiant.core
import DFiant.compiler.ir

export DFOwner.asFE
export DFNet.asFE
extension (dfVal: ir.DFVal)
  def asFE[T <: DFType, M <: DFVal.Modifier]: DFVal[T, M] =
    dfVal.asInstanceOf[DFVal[T, M]]

export DFType.asFE

//TODO: move to DFVal after https://github.com/lampepfl/dotty/issues/13377
//is resolved
type VAL = DFVal.Modifier.VAL
type VAR = DFVal.Modifier.VAR.type
type IN = DFVal.Modifier.IN.type
type OUT = DFVal.Modifier.OUT.type
trait TOKEN
type <>[T <: DFType, M] = M match
  case VAL   => DFValOf[T]
  case VAR   => DFVarOf[T]
  case IN    => DFPortOf[T]
  case OUT   => DFPortOf[T]
  case TOKEN => DFToken.Of[T]
