package DFiant.core
import DFiant.compiler.ir
import DFiant.internals.*
import annotation.targetName

type DFBoolOrBit = OpaqueDFBoolOrBit.DFBoolOrBit
val DFBoolOrBit = OpaqueDFBoolOrBit.DFBoolOrBit

private object OpaqueDFBoolOrBit:
  opaque type DFBoolOrBit <: DFType.Of[ir.DFBoolOrBit] =
    DFType.Of[ir.DFBoolOrBit]
  object DFBoolOrBit:
    type Token = CompanionsDFBoolOrBit.Token
    val Token = CompanionsDFBoolOrBit.Token
//    val DFValTC = CompanionsDFBoolOrBit.DFValTC
//    val Conversions = CompanionsDFBoolOrBit.Conversions
//    val Ops = CompanionsDFBoolOrBit.Ops
//    export CompanionsDFBoolOrBit.Extensions.*
end OpaqueDFBoolOrBit

private object CompanionsDFBoolOrBit:
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
      given DFBoolTokenFromBooleanSing[
          T <: DFBoolOrBit,
          V <: Boolean
      ]: TC[T, ValueOf[V]] with
        def apply(dfType: T, value: ValueOf[V]): Out =
          DFBoolOrBit.Token(dfType, value.value)
      given DFBoolTokenFromBoolean[
          T <: DFBoolOrBit,
          V <: Boolean
      ]: TC[T, V] with
        def apply(dfType: T, value: V): Out =
          DFBoolOrBit.Token(dfType, value)
      given DFBoolTokenFrom1Or0[T <: DFBoolOrBit, V <: 0 | 1]: TC[T, ValueOf[V]]
        with
        def apply(dfType: T, value: ValueOf[V]): Out =
          DFBoolOrBit.Token(dfType, value.value)
      given DFBoolTokenFromToken[
          T <: DFBoolOrBit,
          V <: DFBoolOrBit.Token
      ]: TC[T, V] with
        def apply(dfType: T, value: V): Out =
          DFBoolOrBit.Token(dfType, value.data)
    end TC
  end Token
end CompanionsDFBoolOrBit

//export DFBoolOrBit.Token.Ops.*
//export DFBoolOrBit.Ops.*

opaque type DFBool <: DFBoolOrBit = DFBoolOrBit
final val DFBool = ir.DFBool.asInstanceOf[DFBool]
opaque type DFBit <: DFBoolOrBit = DFBoolOrBit
final val DFBit = ir.DFBit.asInstanceOf[DFBit]
