package DFiant.core
import DFiant.internals.*
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

//TODO: move to DFDecimal after https://github.com/lampepfl/dotty/issues/13377
//is resolved
type DFUInt[W <: Int] = DFDecimal[false, W, 0]
object DFUInt:
  def apply[W <: Int](width: Inlined.Int[W]): DFUInt[W] =
    DFDecimal(false, width, 0)
  type Token[W <: Int] = DFDecimal.Token[false, W, 0]

type DFSInt[W <: Int] = DFDecimal[true, W, 0]
object DFSInt:
  def apply[W <: Int](width: Inlined.Int[W]): DFSInt[W] =
    DFDecimal(true, width, 0)
  type Token[W <: Int] = DFDecimal.Token[true, W, 0]
