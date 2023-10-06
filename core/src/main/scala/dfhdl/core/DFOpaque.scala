package dfhdl.core
import dfhdl.compiler.ir
import dfhdl.compiler.printing.{DefaultPrinter, Printer}
import dfhdl.internals.*
import scala.quoted.*

import scala.annotation.unchecked.uncheckedVariance

type DFOpaque[+TFE <: DFOpaque.Abstract] =
  DFType[ir.DFOpaque, Args1[TFE @uncheckedVariance]]
object DFOpaque:
  protected[core] sealed trait Abstract extends HasTypeName, ir.DFOpaque.CustomId:
    type ActualType <: DFTypeAny
    protected[core] val actualType: ActualType

  abstract class Frontend[A <: DFTypeAny](final protected[core] val actualType: A) extends Abstract:
    type ActualType = A

  given [TFE <: Abstract](using ce: ClassEv[TFE]): DFOpaque[TFE] = DFOpaque(ce.value)

  def apply[TFE <: Abstract](
      t: TFE
  ): DFOpaque[TFE] =
    ir.DFOpaque(t.typeName, t, t.actualType.asIR).asFE[DFOpaque[TFE]]
  extension [A <: DFTypeAny, TFE <: Frontend[A]](dfType: DFOpaque[TFE])
    def actualType: A = dfType.asIR.actualType.asFE[A]

  type Token[TFE <: Abstract] = DFToken[DFOpaque[TFE]]
  object Token:
    def apply[A <: DFTypeAny, TFE <: Frontend[A]](
        tfe: TFE,
        token: A <> TOKEN
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
          TFE <: Abstract,
          RT <: Abstract,
          V <: DFOpaque[RT] <> TOKEN
      ](using RT <:< TFE): TC[DFOpaque[TFE], V] with
        def conv(dfType: DFOpaque[TFE], value: V)(using Ctx): Out =
          value.asTokenOf[DFOpaque[TFE]]

    object Ops:
      extension [A <: DFTypeAny, TFE <: Frontend[A]](
          lhs: DFOpaque[TFE] <> TOKEN
      )
        def actual: A <> TOKEN =
          val lhsIR = lhs.asIR.asInstanceOf[ir.DFToken[ir.DFOpaque]]
          ir.DFToken.forced(lhsIR.dfType.actualType, lhsIR.data).asTokenOf[A]
  end Token

  object Val:
    object TC:
      import DFVal.TC
      given DFOpaqueValFromDFOpaqueVal[
          TFE <: Abstract,
          RT <: Abstract,
          V <: DFOpaque[RT] <> VAL
      ](using RT <:< TFE): TC[DFOpaque[TFE], V] with
        def conv(dfType: DFOpaque[TFE], value: V)(using Ctx): Out =
          value.asValOf[DFOpaque[TFE]]

    object Ops:
      extension [L](inline lhs: L)
        transparent inline def as[Comp <: AnyRef](tfeComp: Comp): Any = ${ asMacro[L, Comp]('lhs) }
      private def asDFVector[A <: DFTypeAny](dfVals: Iterable[DFValOf[A]])(using
          DFC
      ): DFValOf[DFVector[A, Tuple1[Int]]] =
        val dfType = DFVector(dfVals.head.dfType, Tuple1(dfVals.size))
        DFVal.Func(dfType, DFVal.Func.Op.++, dfVals.toList)(using dfc.anonymize)
      extension [A <: DFTypeAny](lhs: Iterable[DFValOf[A]])
        transparent inline def as[Comp <: AnyRef](
            tfeComp: Comp
        )(using DFC): Any = ${
          asMacro[DFValOf[DFVector[A, Tuple1[Int]]], Comp]('{ asDFVector(lhs) })
        }
      private def asMacro[L, Comp <: AnyRef](
          lhs: Expr[L]
      )(using Quotes, Type[L], Type[Comp]): Expr[Any] =
        import quotes.reflect.*
        val tfeTpe = TypeRepr.of[Comp].getCompanionClassTpe
        tfeTpe.baseType(TypeRepr.of[Frontend[_ <: DFTypeAny]].typeSymbol) match
          case AppliedType(_, aTpe :: _) =>
            val aType = aTpe.asTypeOf[DFTypeAny]
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
            val aExpr = '{ $tfe.actualType.asInstanceOf[aType.Underlying] }
            def hasDFVal(tpe: TypeRepr): Boolean =
              tpe.asTypeOf[Any] match
                case '[DFValAny] => true
                case '[NonEmptyTuple] =>
                  tpe.getTupleArgs.exists(hasDFVal)
                case _ => false
            if (hasDFVal(lhsTpe))
              '{
                val tc = compiletime.summonInline[DFVal.TC[aType.Underlying, lhsType.Underlying]]
                val ctx = compiletime.summonInline[tc.Ctx]
                trydf {
                  DFVal.Alias.AsIs(
                    DFOpaque[tfeType.Underlying]($tfe),
                    tc($aExpr, $lhsExpr)(using ctx),
                    Token.forced[tfeType.Underlying]($tfe, _)
                  )(using ctx)
                }(using ctx)
              }
            else
              '{
                val tc =
                  compiletime.summonInline[DFToken.TC[aType.Underlying, lhsType.Underlying]]
                Token.forced[tfeType.Underlying]($tfe, tc($aExpr, $lhsExpr))
              }
            end if
          case _ =>
            report.errorAndAbort("Not a valid opaque type companion.")
        end match
      end asMacro

      extension [AT <: DFTypeAny, TFE <: Frontend[AT], A, C, I](
          lhs: DFVal[DFOpaque[TFE], Modifier[A, C, I]]
      )
        def actual(using DFC): DFVal[AT, Modifier[A, Any, Any]] = // trydf {
          import Token.Ops.{actual => actualToken}
          DFVal.Alias.AsIs(lhs.dfType.actualType, lhs, _.actualToken)
//        }
        // TODO: what to do with the token function?
        // TODO: there is strange result when applying `lhs.actualMap: lhs =>` with the same name
        //       could be a scala compiler bug or a plugin bug (simple examples seem to work fine)
        //       see https://scastie.scala-lang.org/PPXZD3rORxaxQ3PTmin6wg
        @scala.annotation.experimental
        def actualMap(
            f: DFValOf[AT] => DFValOf[AT]
        )(using dfc: DFC, ce: ClassEv[TFE]): DFValOf[DFOpaque[TFE]] =
          DFVal.Alias.AsIs(
            DFOpaque[TFE](ce.value),
            f(lhs.actual),
            x => ???
          )
      end extension
    end Ops
  end Val

end DFOpaque
