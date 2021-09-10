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
  object Of:
    extension [T <: DFType](token: Of[T])
      def dfType: T = token.asIR.dfType.asInstanceOf[T]
      def width(using w: Width[T]): Inlined[w.Out] =
        Inlined.forced[w.Out](token.asIR.width)
  @implicitNotFound("Unsupported token value ${V} for dataflow type ${T}")
  trait TC[T <: DFType, -V] extends GeneralTC[T, V, DFToken]:
    type Out = DFToken.Of[T]
  object TC:
    export DFBoolOrBit.Token.TC.given
    export DFBits.Token.TC.given
    export DFTuple.Token.TC.given

    transparent inline given DFTokenFromBubble[T <: DFType]: TC[T, Bubble] =
      new TC[T, Bubble]:
        def apply(dfType: T, value: Bubble): Out =
          Bubble(dfType)
  end TC

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
      val valueOfTpe = TypeRepr.of[ValueOf]
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

object DFTokenOps:
  extension [T <: DFType](token: DFToken.Of[T])
    def bits(using w: Width[T]): DFToken.Of[DFBits[w.Out]] =
      token.asIR.bits.asTokenOf[DFBits[w.Out]]
end DFTokenOps
