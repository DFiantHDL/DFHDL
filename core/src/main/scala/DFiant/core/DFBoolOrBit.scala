package DFiant.core
import DFiant.compiler.ir
import DFiant.internals.*
import annotation.targetName

type Bit = 0 | 1

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
  type Token = DFToken[DFBoolOrBit]
  object Token:
    extension (token: Token)
      def data: Option[Boolean] = token.asIR.data.asInstanceOf[Option[Boolean]]
    protected[core] def apply[T <: DFBoolOrBit](
        dfType: T,
        data: Option[Boolean]
    ): T <> TOKEN =
      ir.DFToken(dfType.asIR, data).asTokenOf[T]
    protected[core] def apply[T <: DFBoolOrBit](
        dfType: T,
        value: Boolean
    ): T <> TOKEN =
      Token(dfType, Some(value))
    protected[core] def apply[T <: DFBoolOrBit](
        dfType: T,
        value: Bit
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

    trait Candidate[-R]:
      type OutT <: DFBoolOrBit
      def apply(arg: R): DFToken[OutT]
    object Candidate:
      transparent inline given fromBooleanSing[
          R <: Boolean
      ]: Candidate[ValueOf[R]] = new Candidate[ValueOf[R]]:
        type OutT = DFBool
        def apply(arg: ValueOf[R]): DFToken[DFBool] =
          DFBoolOrBit.Token(DFBool, arg.value)
      transparent inline given fromBoolean: Candidate[Boolean] =
        new Candidate[Boolean]:
          type OutT = DFBool
          def apply(arg: Boolean): DFToken[DFBool] =
            DFBoolOrBit.Token(DFBool, arg)
      transparent inline given fromBit[
          R <: Bit
      ]: Candidate[ValueOf[R]] = new Candidate[ValueOf[R]]:
        type OutT = DFBit
        def apply(arg: ValueOf[R]): DFToken[DFBit] =
          DFBoolOrBit.Token(DFBit, arg.value)
      transparent inline given fromDFBoolOrBitToken[
          T <: DFBoolOrBit
      ]: Candidate[DFToken[T]] = new Candidate[DFToken[T]]:
        type OutT = T
        def apply(arg: DFToken[T]): DFToken[T] = arg
    end Candidate

    object TC:
      import DFToken.TC
      given DFBoolTokenFromCandidate[T <: DFBoolOrBit, R](using
          ic: Candidate[R]
      ): TC[T, R] with
        def apply(dfType: T, arg: R): Out =
          val tokenArg = ic(arg)
          val tokenOut = (dfType, tokenArg.dfType) match
            case (DFBit, DFBool) => DFBoolOrBit.Token(DFBit, tokenArg.data)
            case (DFBool, DFBit) => DFBoolOrBit.Token(DFBool, tokenArg.data)
            case _               => tokenArg
          tokenOut.asIR.asTokenOf[T]
    end TC
  end Token
end CompanionsDFBoolOrBit

//export DFBoolOrBit.Token.Ops.*
//export DFBoolOrBit.Ops.*

opaque type DFBool <: DFBoolOrBit = DFBoolOrBit
final val DFBool = ir.DFBool.asInstanceOf[DFBool]
opaque type DFBit <: DFBoolOrBit = DFBoolOrBit
final val DFBit = ir.DFBit.asInstanceOf[DFBit]
given CanEqual[DFBoolOrBit, DFBoolOrBit] = CanEqual.derived
