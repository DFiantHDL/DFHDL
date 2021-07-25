package DFiant.core
import DFiant.compiler.ir
import DFiant.internals.*
import annotation.targetName

//TODO: simplify after https://github.com/lampepfl/dotty/issues/13120 is fixed
opaque type DFBoolOrBit <: DFType.Of[
  DFiant.compiler.ir.DFBoolOrBit
] = DFType.Of[DFiant.compiler.ir.DFBoolOrBit]
object DFBoolOrBit:
  type Data = Option[Boolean]
  type Token = DFToken.Of[DFBoolOrBit]
  object Token:
    extension (token: Token)
      def data: Option[Boolean] = token.asIR.data.asInstanceOf[Option[Boolean]]
    protected[core] def apply[T <: DFBoolOrBit](
        dfType: T,
        data: Option[Boolean]
    ): T <> TOKEN =
      ir.DFToken(dfType.asIR, data).asInstanceOf[T <> TOKEN]
    protected[core] def apply[T <: DFBoolOrBit](
        dfType: T,
        value: Boolean
    ): T <> TOKEN =
      Token(dfType, Some(value))
    protected[core] def apply[T <: DFBoolOrBit](
        dfType: T,
        value: 0 | 1
    ): T <> TOKEN =
      Token(dfType, value > 0)
    protected[core] def apply[T <: DFBoolOrBit](
        dfType: T,
        value: Bubble
    ): T <> TOKEN =
      Token(dfType, None)

    object Conversions:
      import DFToken.TC
      given fromSingletonToDFBoolToken[V <: Singleton](using
          tc: DFToken.TC[DFBool, ValueOf[V]]
      ): Conversion[V, DFBool <> TOKEN] = value => tc(DFBool, ValueOf(value))
      given fromSingletonToDFBitToken[V <: Singleton](using
          tc: DFToken.TC[DFBit, ValueOf[V]]
      ): Conversion[V, DFBit <> TOKEN] = value => tc(DFBit, ValueOf(value))
      given fromNonSingletonToDFBoolToken[V](using
          tc: DFToken.TC[DFBool, V]
      ): Conversion[V, DFBool <> TOKEN] = value => tc(DFBool, value)
      given fromNonSingletonToDFBitToken[V](using
          tc: DFToken.TC[DFBit, V]
      ): Conversion[V, DFBit <> TOKEN] = value => tc(DFBit, value)
    end Conversions

//    object Ops:
//      extension (token: Token)
//        def asBool: Token = Token(DFBool, token.data)
//        def asBit: Token = Token(DFBit, token.data)
//    end Ops

    object TC:
      import DFToken.TC
      transparent inline given DFBoolTokenFromBooleanSing[
          T <: DFBoolOrBit,
          V <: Boolean
      ]: TC[T, ValueOf[V]] =
        new TC[T, ValueOf[V]]:
          type Out = T <> TOKEN
          def apply(dfType: T, value: ValueOf[V]): Out =
            DFBoolOrBit.Token(dfType, value.value)
      transparent inline given DFBoolTokenFromBoolean[
          T <: DFBoolOrBit,
          V <: Boolean
      ]: TC[T, V] =
        new TC[T, V]:
          type Out = T <> TOKEN
          def apply(dfType: T, value: V): Out =
            DFBoolOrBit.Token(dfType, value)
      transparent inline given DFBoolTokenFrom1Or0[T <: DFBoolOrBit, V <: 0 | 1]
          : TC[T, ValueOf[V]] =
        new TC[T, ValueOf[V]]:
          type Out = T <> TOKEN
          def apply(dfType: T, value: ValueOf[V]): Out =
            DFBoolOrBit.Token(dfType, value.value)
      transparent inline given DFBoolTokenFromToken[
          T <: DFBoolOrBit,
          V <: DFBoolOrBit.Token
      ]: TC[T, V] =
        new TC[T, V]:
          type Out = T <> TOKEN
          def apply(dfType: T, value: V): Out =
            DFBoolOrBit.Token(dfType, value.data)
    end TC
  end Token

//  object Ops:
////    extension (dfVal: DFBoolOrBit <> VAL)
////      def asBool(using DFC): DFBoolOrBit <> VAL = dfVal.as(DFBool)
////      def asBit(using DFC): DFBoolOrBit <> VAL = dfVal.as(DFBit)
//  end Ops
end DFBoolOrBit

//export DFBoolOrBit.Token.Ops.*
//export DFBoolOrBit.Ops.*

opaque type DFBool <: DFBoolOrBit = DFBoolOrBit
final val DFBool = ir.DFBool.asInstanceOf[DFBool]
opaque type DFBit <: DFBoolOrBit = DFBoolOrBit
final val DFBit = ir.DFBit.asInstanceOf[DFBit]
