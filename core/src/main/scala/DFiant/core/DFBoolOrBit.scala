package DFiant.core
import DFiant.compiler.ir
import DFiant.internals.*

opaque type DFBoolOrBit <: DFType.Of[ir.DFBoolOrBit] = DFType.Of[ir.DFBoolOrBit]
object DFBoolOrBit:
  opaque type Token <: DFToken.Of[DFBoolOrBit, Option[Boolean]] =
    DFToken.Of[DFBoolOrBit, Option[Boolean]]

type DFBool = DFBoolOrBit
final val DFBool = ir.DFBool.asInstanceOf[DFBoolOrBit]
type DFBit = DFBoolOrBit
final val DFBit = ir.DFBit.asInstanceOf[DFBoolOrBit]
