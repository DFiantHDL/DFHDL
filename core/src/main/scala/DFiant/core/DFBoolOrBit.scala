package DFiant.core
import DFiant.compiler.ir
import DFiant.internals.*
import annotation.targetName
opaque type DFBoolOrBit <: DFType.Of[ir.DFBoolOrBit] with DFBoolOrBit.HasToken =
  DFType.Of[ir.DFBoolOrBit] with DFBoolOrBit.HasToken
object DFBoolOrBit:
  type Data = Option[Boolean]
  trait HasToken { type Token = DFBoolOrBit.Token }
  type Token = DFToken.Of[DFBoolOrBit]
  object Token:
    extension (token: Token)
      def data: Option[Boolean] = token.asIR.data.asInstanceOf[Option[Boolean]]
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

    object Ops:
      extension (token: Token)
        def asBool: Token = Token(DFBool, token.data)
        def asBit: Token = Token(DFBit, token.data)
    end Ops

    object TC:
      import DFToken.TC
      given DFBoolTokenFromBooleanSing[V <: Boolean]
          : TC[DFBoolOrBit, ValueOf[V]] =
        new TC[DFBoolOrBit, ValueOf[V]]:
          def apply(dfType: DFBoolOrBit, value: ValueOf[V]): Out =
            DFBoolOrBit.Token(dfType, value.value)
      given DFBoolTokenFromBoolean[V <: Boolean]: TC[DFBoolOrBit, V] =
        new TC[DFBoolOrBit, V]:
          def apply(dfType: DFBoolOrBit, value: V): Out =
            DFBoolOrBit.Token(dfType, value)
      given DFBoolTokenFrom1Or0[V <: 0 | 1]: TC[DFBoolOrBit, ValueOf[V]] =
        new TC[DFBoolOrBit, ValueOf[V]]:
          def apply(dfType: DFBoolOrBit, value: ValueOf[V]): Out =
            DFBoolOrBit.Token(dfType, value.value)
      given DFBoolTokenFromToken[V <: DFBoolOrBit.Token]: TC[DFBoolOrBit, V] =
        new TC[DFBoolOrBit, V]:
          def apply(dfType: DFBoolOrBit, value: V): Out =
            DFBoolOrBit.Token(dfType, value.data)
    end TC
  end Token

  object DFValTC:
    import DFVal.TC
    transparent inline given DFBoolOrBitArg[M <: DFVal.Modifier](using
        DFC
    ): TC[DFBoolOrBit, DFVal[DFBoolOrBit, M]] =
      new TC[DFBoolOrBit, DFVal[DFBoolOrBit, M]]:
        type Out = DFBoolOrBit
        def apply(
            dfType: DFBoolOrBit,
            value: DFVal[DFBoolOrBit, M]
        ): DFValOf[Out] =
          val updated = (dfType.asIR, value.asIR.dfType) match
            case (ir.DFBool, ir.DFBit) => value.asBool
            case (ir.DFBit, ir.DFBool) => value.asBit
            case _                     => value
          updated.asIR.asValOf[Out]
  end DFValTC

  object Ops:
    extension (dfVal: DFBoolOrBit <> VAL)
      def asBool(using DFC): DFBoolOrBit <> VAL = ??? //dfVal.as(DFBool)
      def asBit(using DFC): DFBoolOrBit <> VAL = ??? //dfVal.as(DFBit)
  end Ops
end DFBoolOrBit

export DFBoolOrBit.Token.Ops.*
export DFBoolOrBit.Ops.*

type DFBool = DFBoolOrBit
final val DFBool = ir.DFBool.asInstanceOf[DFBoolOrBit]
type DFBit = DFBoolOrBit
final val DFBit = ir.DFBit.asInstanceOf[DFBoolOrBit]
