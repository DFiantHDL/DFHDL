package DFiant.core
import DFiant.compiler.printing.Printer
import DFiant.compiler.ir
import ir.DFVal.Func.{Op => FuncOp}
import DFiant.internals.*

import scala.quoted.*
import scala.annotation.implicitNotFound
import scala.annotation.unchecked.uncheckedVariance

final class DFToken[+T <: DFType](val value: ir.DFType.Token) extends AnyVal:
  inline def ==[R](inline that: R)(using es: Exact.Summon[R, that.type])(using
      c: DFToken.Compare[T @uncheckedVariance, es.Out, FuncOp.===.type, false]
  ): DFToken[DFBool] = c(this, es(that))
  inline def !=[R](inline that: R)(using es: Exact.Summon[R, that.type])(using
      c: DFToken.Compare[T @uncheckedVariance, es.Out, FuncOp.=!=.type, false]
  ): DFToken[DFBool] = c(this, es(that))
  def ==[R <: DFType](that: DFValOf[R])(using
      dfc: DFC,
      op: DFVal.Compare[R, DFToken[T], FuncOp.===.type, true]
  ): DFBool <> VAL = op(that, this)
  def !=[R <: DFType](that: DFValOf[R])(using
      dfc: DFC,
      op: DFVal.Compare[R, DFToken[T], FuncOp.=!=.type, true]
  ): DFBool <> VAL = op(that, this)
  override def toString: String = value.toString
end DFToken

type DFTokenAny = DFToken[DFType]
extension (tokenIR: ir.DFType.Token)
  def asTokenOf[T <: DFType]: DFToken[T] = DFToken[T](tokenIR)

object DFToken:
  //Implicit conversions for tokens
  export DFBoolOrBit.Token.Conversions.given
  export DFBits.Token.Conversions.given

  protected[core] def bubble[T <: DFType](dfType: T): DFToken[T] =
    ir.DFType.Token.bubble(dfType.asIR).asTokenOf[T]
  extension (token: DFTokenAny)
    def asIR: ir.DFType.Token = token.value
    def codeString(using printer: Printer): String = printer.csDFToken(asIR)

  @implicitNotFound("Unsupported token value ${V} for dataflow type ${T}")
  trait TC[T <: DFType, -V] extends GeneralTC[T, V, DFTokenAny]:
    type Out = DFToken[T]
  object TC:
    export DFBoolOrBit.Token.TC.given
    export DFBits.Token.TC.given
    export DFDecimal.Token.TC.given
    export DFTuple.Token.TC.given

    transparent inline given DFTokenFromBubble[T <: DFType]: TC[T, Bubble] =
      (dfType: T, value: Bubble) => Bubble(dfType)
  end TC

  @implicitNotFound("Cannot compare token of ${T} with value of ${V}")
  trait Compare[T <: DFType, -V, Op <: FuncOp, C <: Boolean]:
    def apply(token: DFToken[T], arg: V): DFToken[DFBool]
  object Compare:
    export DFBoolOrBit.Token.Compare.given
    export DFDecimal.Token.Compare.given
    export DFBits.Token.Compare.given

  object Ops:
    extension [T <: DFType](token: DFToken[T])
      def bits(using w: Width[T]): DFToken[DFBits[w.Out]] =
        token.asIR.bits.asTokenOf[DFBits[w.Out]]
  end Ops

  trait Value[T <: DFType]:
    type Out <: DFTokenAny
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
