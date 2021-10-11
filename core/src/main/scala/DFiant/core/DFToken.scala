package DFiant.core
import DFiant.compiler.printing.Printer
import DFiant.compiler.ir
import DFiant.internals.*

import scala.quoted.*
import scala.annotation.implicitNotFound

opaque type DFToken = ir.DFType.Token
extension (token: ir.DFType.Token)
  def asTokenOf[T <: DFType]: DFToken.Of[T] = token.asInstanceOf[DFToken.Of[T]]
object DFToken:
  //Implicit conversions for tokens
  export DFBoolOrBit.Token.Conversions.given
  export DFBits.Token.Conversions.given

  protected[core] def bubble[T <: DFType](dfType: T): DFToken.Of[T] =
    ir.DFType.Token.bubble(dfType.asIR)
  extension (of: DFToken)
    def asIR: ir.DFType.Token = of
    def codeString(using printer: Printer): String = printer.csDFToken(asIR)

  opaque type Of[+T <: DFType] <: DFToken = DFToken
  @implicitNotFound("Unsupported token value ${V} for dataflow type ${T}")
  trait TC[T <: DFType, -V] extends GeneralTC[T, V, DFToken]:
    type Out = DFToken.Of[T]
  object TC:
    export DFBoolOrBit.Token.TC.given
    export DFBits.Token.TC.given
    export DFDecimal.Token.TC.given
    export DFTuple.Token.TC.given

    transparent inline given DFTokenFromBubble[T <: DFType]: TC[T, Bubble] =
      (dfType: T, value: Bubble) => Bubble(dfType)
  end TC

  @implicitNotFound("Cannot compare token of ${T} with value of ${V}")
  trait Compare[T <: DFType, -V, Op <: ir.DFVal.Func.Op]:
    def apply(token: Of[T], arg: V): Of[DFBool]
  object Compare:
    export DFDecimal.Token.Compare.given

  val Ops = CompanionsDFToken.Ops
  trait Value[T <: DFType]:
    type Out <: DFToken
    def apply(dfType: T): Out
  object Value:
    transparent inline implicit def fromValue[T <: DFType, V](
        inline value: V
    ): Value[T] = ${ fromValueMacro[T, V]('value) }

    def fromValueMacro[T <: DFType, V](
        value: Expr[V]
    )(using Quotes, Type[T], Type[V]): Expr[Value[T]] =
      import quotes.reflect.*
      val term = value.asTerm.underlyingArgument.exactTerm
      val tpe = term.tpe.asTypeOf[Any]
      '{
        val tc = compiletime.summonInline[DFToken.TC[T, tpe.Underlying]]
        new Value[T]:
          type Out = tc.Out
          def apply(dfType: T): Out =
            tc(dfType, ${ term.asExpr })
      }
    end fromValueMacro
  end Value
end DFToken

private object CompanionsDFToken:
  object Ops:
    extension [T <: DFType](token: DFToken.Of[T])
      def bits(using w: Width[T]): DFToken.Of[DFBits[w.Out]] =
        token.asIR.bits.asTokenOf[DFBits[w.Out]]
  end Ops
