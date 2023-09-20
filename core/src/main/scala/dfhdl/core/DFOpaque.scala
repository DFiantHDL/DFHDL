package dfhdl.core
import dfhdl.compiler.ir
import dfhdl.compiler.printing.{DefaultPrinter, Printer}
import dfhdl.internals.*
import scala.quoted.*

import scala.annotation.unchecked.uncheckedVariance

type DFOpaque[+T <: DFOpaque.Abstract] =
  DFType[ir.DFOpaque, Args1[T @uncheckedVariance]]
object DFOpaque:
  protected[core] sealed trait Abstract extends HasTypeName, ir.DFOpaque.CustomId:
    type ActualType <: DFTypeAny
    protected[core] val actualType: ActualType

  abstract class Frontend[T <: DFTypeAny](final protected[core] val actualType: T) extends Abstract:
    type ActualType = T

  given [T <: Abstract](using ce: ClassEv[T]): DFOpaque[T] = DFOpaque(ce.value)

  def apply[T <: Abstract](
      t: T
  ): DFOpaque[T] =
    ir.DFOpaque(t.typeName, t, t.actualType.asIR).asFE[DFOpaque[T]]
  extension [T <: DFTypeAny, TFE <: Frontend[T]](dfType: DFOpaque[TFE])
    def actualType: T = dfType.asIR.actualType.asFE[T]

  type Token[T <: Abstract] = DFToken[DFOpaque[T]]
  object Token:
    def apply[T <: DFTypeAny, TFE <: Frontend[T]](
        tfe: TFE,
        token: T <> TOKEN
    ): Token[TFE] =
      ir.DFToken(DFOpaque(tfe).asIR)(token.asIR.data).asTokenOf[DFOpaque[TFE]]
    def forced[TFE <: Abstract](
        tfe: TFE,
        token: DFTokenAny
    ): Token[TFE] =
      ir.DFToken(DFOpaque(tfe).asIR)(token.asIR.data).asTokenOf[DFOpaque[TFE]]

    object TC:
      import DFToken.TC
      given DFOpaqueTokenFromDFOpaqueToken[
          T <: Abstract,
          RT <: Abstract,
          V <: DFOpaque[RT] <> TOKEN
      ](using RT <:< T): TC[DFOpaque[T], V] with
        def conv(dfType: DFOpaque[T], value: V)(using Ctx): Out =
          value.asTokenOf[DFOpaque[T]]

    object Ops:
      extension [T <: DFTypeAny, TFE <: Frontend[T]](
          lhs: DFOpaque[TFE] <> TOKEN
      )
        def actual: T <> TOKEN =
          val lhsIR = lhs.asIR.asInstanceOf[ir.DFToken[ir.DFOpaque]]
          ir.DFToken.forced(lhsIR.dfType.actualType, lhsIR.data).asTokenOf[T]
  end Token

  object Val:
    object TC:
      import DFVal.TC
      given DFOpaqueValFromDFOpaqueVal[
          T <: Abstract,
          RT <: Abstract,
          V <: DFOpaque[RT] <> VAL
      ](using RT <:< T): TC[DFOpaque[T], V] with
        def conv(dfType: DFOpaque[T], value: V)(using Ctx): Out =
          value.asValOf[DFOpaque[T]]

    object Ops:
      extension [L](inline lhs: L)
        transparent inline def as[Comp <: AnyRef](tfeComp: Comp): Any = ${ asMacro[L, Comp]('lhs) }
      private def asDFVector[T <: DFTypeAny](dfVals: Vector[DFValOf[T]])(using
          DFC
      ): DFValOf[DFVector[T, Tuple1[Int]]] =
        val dfType = DFVector(dfVals.head.dfType, Tuple1(dfVals.length))
        DFVal.Func(dfType, DFVal.Func.Op.++, dfVals.toList)(using dfc.anonymize)
      extension [T <: DFTypeAny](lhs: Vector[DFValOf[T]])
        transparent inline def as[Comp <: AnyRef](
            tfeComp: Comp
        )(using DFC): Any = ${
          asMacro[DFValOf[DFVector[T, Tuple1[Int]]], Comp]('{ asDFVector(lhs) })
        }
      private def asMacro[L, Comp <: AnyRef](
          lhs: Expr[L]
      )(using Quotes, Type[L], Type[Comp]): Expr[Any] =
        import quotes.reflect.*
        val tfeTpe = TypeRepr.of[Comp].getCompanionClassTpe
        tfeTpe.baseType(TypeRepr.of[Frontend[_ <: DFTypeAny]].typeSymbol) match
          case AppliedType(_, tTpe :: _) =>
            val tType = tTpe.asTypeOf[DFTypeAny]
            val tfeType = tfeTpe.asTypeOf[Abstract]
            val tfe = '{
              compiletime
                .summonInline[ClassEv[tfeType.Underlying]]
                .value
            }
            val lhsTerm = lhs.asTerm.exactTerm
            val lhsTpe = lhsTerm.tpe
            val lhsExpr = lhsTerm.asExpr
            val lhsType = lhsTpe.asTypeOf[Any]
            val tExpr = '{ $tfe.actualType.asInstanceOf[tType.Underlying] }
            def hasDFVal(tpe: TypeRepr): Boolean =
              tpe.asTypeOf[Any] match
                case '[DFValAny] => true
                case '[NonEmptyTuple] =>
                  tpe.getTupleArgs.exists(hasDFVal)
                case _ => false
            if (hasDFVal(lhsTpe))
              '{
                val tc = compiletime.summonInline[DFVal.TC[tType.Underlying, lhsType.Underlying]]
                val ctx = compiletime.summonInline[tc.Ctx]
                trydf {
                  DFVal.Alias.AsIs(
                    DFOpaque[tfeType.Underlying]($tfe),
                    tc($tExpr, $lhsExpr)(using ctx),
                    Token.forced[tfeType.Underlying]($tfe, _)
                  )(using ctx)
                }(using ctx)
              }
            else
              '{
                val tc =
                  compiletime.summonInline[DFToken.TC[tType.Underlying, lhsType.Underlying]]
                Token.forced[tfeType.Underlying]($tfe, tc($tExpr, $lhsExpr))
              }
            end if
          case _ =>
            report.errorAndAbort("Not a valid opaque type companion.")
        end match
      end asMacro

      extension [T <: DFTypeAny, TFE <: Frontend[T], A, C, I](
          lhs: DFVal[DFOpaque[TFE], Modifier[A, C, I]]
      )
        def actual(using DFC): DFVal[T, Modifier[A, Any, Any]] = // trydf {
          import Token.Ops.{actual => actualToken}
          DFVal.Alias.AsIs(lhs.dfType.actualType, lhs, _.actualToken)
//        }
    end Ops
  end Val

end DFOpaque
