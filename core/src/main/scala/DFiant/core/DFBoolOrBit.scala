package DFiant.core
import DFiant.compiler.ir
import DFiant.internals.*

opaque type DFBoolOrBit <: DFType.Of[ir.DFBoolOrBit] = DFType.Of[ir.DFBoolOrBit]
object DFBoolOrBit:
  opaque type Token <: DFToken.Of[DFBoolOrBit, Option[Boolean]] =
    DFToken.Of[DFBoolOrBit, Option[Boolean]]
  object Token:
    def apply(dfType: DFBoolOrBit, value: Boolean): Token =
      ir.DFToken(dfType.asIR, Some(value)).asInstanceOf[Token]
    def apply(dfType: DFBoolOrBit, value: 0 | 1): Token =
      Token(dfType, value > 0)
    def bubble(dfType: DFBoolOrBit): Token =
      ir.DFToken(dfType.asIR, None).asInstanceOf[Token]

type DFBool = DFBoolOrBit
final val DFBool = ir.DFBool.asInstanceOf[DFBoolOrBit]
type DFBit = DFBoolOrBit
final val DFBit = ir.DFBit.asInstanceOf[DFBoolOrBit]
