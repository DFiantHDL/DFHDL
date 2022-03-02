package DFiant.core
import DFiant.compiler.ir
import DFiant.compiler.printing.{DefaultPrinter, Printer}
import DFiant.internals.*
import scala.quoted.*

import scala.annotation.unchecked.uncheckedVariance

type DFOpaque[+T <: DFOpaque.Abstract] =
  DFType[ir.DFOpaque, Args1[T @uncheckedVariance]]
object DFOpaque:
  protected[core] sealed trait Abstract extends HasTypeName:
    type ActualType <: DFTypeAny
    val actualType: ActualType
  class Frontend[T <: DFTypeAny](final val actualType: T) extends Abstract:
    type ActualType = T

  given [T <: Abstract](using ValueOf[T]): DFOpaque[T] = DFOpaque(valueOf[T])

  def apply[T <: Abstract](
      t: T
  ): DFOpaque[T] =
    ir.DFOpaque(t.typeName, t.actualType.asIRForced).asFE[DFOpaque[T]]
  extension [T <: DFTypeAny, TFE <: Frontend[T]](dfType: DFOpaque[TFE])
    def actualType: T = dfType.asIRForced.actualType.asFE[T]

//  private def checkAs[T <: DFTypeAny](tokenDFType: T, actualDFType: T): Unit =
//    given Printer = DefaultPrinter
//    assert(
//      tokenDFType == actualDFType,
//      s"The token type (${tokenDFType.codeString}) does not match the actual opaque type ${actualDFType.codeString}"
//    )

  type Token[T <: Abstract] = DFToken[DFOpaque[T]]
  object Token:
    def apply[T <: DFTypeAny, TFE <: Frontend[T]](
        tfe: TFE,
        token: T <> TOKEN
    ): Token[TFE] =
      ir.DFToken(DFOpaque(tfe).asIRForced)(token.asIRForced.data).asTokenOf[DFOpaque[TFE]]

    object Ops:
      extension [T <: DFTypeAny, TFE <: Frontend[T]](
          lhs: DFOpaque[TFE] <> TOKEN
      )
        def actual: T <> TOKEN =
          lhs.asIRForced.data.asInstanceOf[ir.DFTokenAny].asTokenOf[T]
  end Token

  object Val:
    object Ops:
      import ir.DFVal.Modifier
      extension [L](inline lhs: L)
        transparent inline def as[T <: DFTypeAny, TFE <: Frontend[T]](
            tfe: TFE
        ): Any = ${ asMacro[L, T, TFE]('lhs, 'tfe) }
      private def asMacro[L, T <: DFTypeAny, TFE <: Frontend[T]](
          lhs: Expr[L],
          tfe: Expr[TFE]
      )(using Quotes, Type[L], Type[T], Type[TFE]): Expr[Any] =
        import quotes.reflect.*
        val tTpe = TypeRepr.of[T]
        val lhsTerm = lhs.asTerm.exactTerm
        val lhsTpe = lhsTerm.tpe
        val lhsExpr = lhsTerm.asExpr
        val lhsType = lhsTpe.asTypeOf[Any]
        val tExpr = '{ $tfe.actualType }
        def hasDFVal(tpe: TypeRepr): Boolean =
          tpe.asTypeOf[Any] match
            case '[DFValAny] => true
            case '[ValueOf[t]] =>
              hasDFVal(TypeRepr.of[t])
            case '[NonEmptyTuple] =>
              tpe.getTupleArgs.exists(hasDFVal)
            case _ => false
        if (hasDFVal(lhsTpe))
          '{
            val tc = compiletime.summonInline[DFVal.TC[T, lhsType.Underlying]]
            DFVal.Alias.AsIs(
              DFOpaque($tfe),
              tc($tExpr, $lhsExpr),
              Token($tfe, _)
            )(using compiletime.summonInline[DFC])
          }
        else
          '{
            val tc =
              compiletime.summonInline[DFToken.TC[T, lhsType.Underlying]]
            Token($tfe, tc($tExpr, $lhsExpr))
          }
      end asMacro

      extension [T <: DFTypeAny, TFE <: Frontend[T], A, C, I](
          lhs: DFVal[DFOpaque[TFE], Modifier[A, C, I]]
      )
        def actual(using DFC): DFVal[T, Modifier[A, Any, Any]] =
          import Token.Ops.{actual => actualToken}
          DFVal.Alias.AsIs(lhs.dfType.actualType, lhs, _.actualToken)
    end Ops
  end Val

end DFOpaque
