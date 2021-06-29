package DFiant.core
import DFiant.compiler.ir
import DFiant.internals.*

opaque type DFBoolOrBit <: DFType.Of[ir.DFBoolOrBit] = DFType.Of[ir.DFBoolOrBit]
object DFBoolOrBit:
  opaque type Token <: DFToken.Of[DFBoolOrBit, Option[Boolean]] =
    DFToken.Of[DFBoolOrBit, Option[Boolean]]
  object Token:
    protected[core] def apply(
        dfType: DFBoolOrBit,
        data: Option[Boolean]
    ): Token =
      ir.DFToken(dfType.asIR, data).asInstanceOf[Token]
    protected[core] def apply(dfType: DFBoolOrBit, value: Boolean): Token =
      Token(dfType, Some(value))
    protected[core] def apply(dfType: DFBoolOrBit, value: 0 | 1): Token =
      Token(dfType, value > 0)
    protected[core] def apply(dfType: DFBoolOrBit, value: Bubble): Token =
      Token(dfType, None)

    object TC:
      import DFToken.TC
      transparent inline given DFBoolTokenFromBubble[V <: Bubble]
          : TC[DFBoolOrBit, V] =
        new TC[DFBoolOrBit, V]:
          type Out = DFBoolOrBit.Token
          def apply(dfType: DFBoolOrBit, value: V): Out =
            DFBoolOrBit.Token(dfType, value)
      transparent inline given DFBoolTokenFromBoolean[V <: Boolean]
          : TC[DFBoolOrBit, ValueOf[V]] =
        new TC[DFBoolOrBit, ValueOf[V]]:
          type Out = DFBoolOrBit.Token
          def apply(dfType: DFBoolOrBit, value: ValueOf[V]): Out =
            DFBoolOrBit.Token(dfType, value.value)
      transparent inline given DFBoolTokenFrom1Or0[V <: 0 | 1]
          : TC[DFBoolOrBit, ValueOf[V]] =
        new TC[DFBoolOrBit, ValueOf[V]]:
          type Out = DFBoolOrBit.Token
          def apply(dfType: DFBoolOrBit, value: ValueOf[V]): Out =
            DFBoolOrBit.Token(dfType, value.value)
      transparent inline given DFBoolTokenFromToken[V <: DFBoolOrBit.Token]
          : TC[DFBoolOrBit, V] =
        new TC[DFBoolOrBit, V]:
          type Out = DFBoolOrBit.Token
          def apply(dfType: DFBoolOrBit, value: V): Out =
            DFBoolOrBit.Token(dfType, value.data)

type DFBool = DFBoolOrBit
final val DFBool = ir.DFBool.asInstanceOf[DFBoolOrBit]
type DFBit = DFBoolOrBit
final val DFBit = ir.DFBit.asInstanceOf[DFBoolOrBit]
