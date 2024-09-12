package dfhdl.core
import dfhdl.compiler.ir
import dfhdl.compiler.printing.{DefaultPrinter, Printer}
import dfhdl.internals.*
import scala.quoted.*

import scala.annotation.unchecked.uncheckedVariance
import scala.annotation.targetName

type DFOpaque[+TFE <: DFOpaque.Abstract] =
  DFType[ir.DFOpaque, Args1[TFE @uncheckedVariance]]
object DFOpaque:
  protected[core] sealed trait Abstract extends HasTypeName, ir.DFOpaque.Id:
    type ActualType <: DFTypeAny
    protected[core] val actualType: ActualType
  object Abstract:
    inline implicit def fromComp[TFE <: Abstract, Comp <: AnyRef](tfeComp: Comp)(implicit
        cc: CaseClass[Comp, TFE]
    ): cc.CC = compiletime.summonInline[ClassEv[cc.CC]].value
    def fromCompMacro[TFE <: Abstract, Comp <: AnyRef](using
        Quotes,
        Type[TFE],
        Type[Comp]
    ): Expr[TFE] =
      import quotes.reflect.*
      val compTpe = TypeRepr.of[Comp]
      val tfeTpe = compTpe.getCompanionClassTpe
      val tfeType = tfeTpe.asTypeOf[TFE]
      '{
        compiletime.summonInline[ClassEv[tfeType.Underlying]].value
      }
  end Abstract

  abstract class Frontend[A <: DFTypeAny](final protected[core] val actualType: A) extends Abstract:
    type ActualType = A

  abstract class Magnet[A <: DFTypeAny](actualType: A)
      extends Frontend[A](actualType),
        ir.DFOpaque.MagnetId
  abstract class Clk extends Magnet[DFBit](DFBit), ir.DFOpaque.Clk
  abstract class Rst extends Magnet[DFBit](DFBit), ir.DFOpaque.Rst

  given [TFE <: Abstract](using ce: ClassEv[TFE], dfc: DFC): DFOpaque[TFE] = DFOpaque(ce.value)

  def apply[TFE <: Abstract](
      t: TFE
  )(using DFC): DFOpaque[TFE] = trydf:
    ir.DFOpaque(
      t.typeName,
      t,
      t.actualType.asIR.dropUnreachableRefs(allowDesignParamRefs = false)
    ).asFE[DFOpaque[TFE]]
  extension [A <: DFTypeAny, TFE <: Frontend[A]](dfType: DFOpaque[TFE])
    def actualType: A = dfType.asIR.actualType.asFE[A]
    def opaqueType: TFE = dfType.asIR.id.asInstanceOf[TFE]

  object Val:
    object TC:
      import DFVal.TC
      given DFOpaqueValFromDFOpaqueVal[
          TFE <: Abstract,
          RT <: Abstract,
          RP,
          V <: DFValTP[DFOpaque[RT], RP]
      ](using RT <:< TFE): TC[DFOpaque[TFE], V] with
        type OutP = RP
        def conv(dfType: DFOpaque[TFE], value: V)(using DFC): Out =
          value.asValTP[DFOpaque[TFE], RP]

    object Ops:
      extension [L](inline lhs: L)
        transparent inline def as[Comp <: AnyRef](tfeComp: Comp): Any = ${
          asMacro[L, Comp]('lhs, 'tfeComp)
        }
      private def asDFVector[A <: DFTypeAny](dfVals: Iterable[DFValOf[A]])(using
          DFC
      ): DFValOf[DFVector[A, Tuple1[Int]]] =
        val dfType = DFVector[A, Tuple1[Int]](dfVals.head.dfType, List(dfVals.size))
        DFVal.Func(dfType, DFVal.Func.Op.++, dfVals.toList)
      extension [A <: DFTypeAny](lhs: Iterable[DFValOf[A]])
        transparent inline def as[Comp <: AnyRef](
            tfeComp: Comp
        )(using DFC): Any = ${
          asMacro[DFValOf[DFVector[A, Tuple1[Int]]], Comp]('{ asDFVector(lhs) }, 'tfeComp)
        }
      private def asMacro[L, Comp <: AnyRef](
          lhs: Expr[L],
          tfeComp: Expr[Comp]
      )(using Quotes, Type[L], Type[Comp]): Expr[Any] =
        import quotes.reflect.*
        val compTpe = TypeRepr.of[Comp]
        val (tfe, tfeTpe) =
          if (compTpe <:< TypeRepr.of[Abstract]) (tfeComp.asExprOf[Abstract], compTpe)
          else
            val tfeTpe = compTpe.getCompanionClassTpe
            val tfeType = tfeTpe.asTypeOf[Abstract]
            val tfe = '{
              compiletime
                .summonInline[ClassEv[tfeType.Underlying]]
                .value
            }
            (tfe, tfeTpe)
        val tfeType = tfeTpe.asTypeOf[Abstract]
        tfeTpe.baseType(TypeRepr.of[Frontend[? <: DFTypeAny]].typeSymbol) match
          case AppliedType(_, aTpe :: _) =>
            val aType = aTpe.asTypeOf[DFTypeAny]
            val lhsExactInfo = lhs.exactInfo
            val pType = lhsExactInfo.exactTpe.isConstTpe.asTypeOf[Any]
            val aExpr = '{ $tfe.actualType.asInstanceOf[aType.Underlying] }
            '{
              val tc = compiletime.summonInline[DFVal.TC[aType.Underlying, lhsExactInfo.Underlying]]
              val dfc = compiletime.summonInline[DFC]
              DFVal.Alias.AsIs(
                DFOpaque[tfeType.Underlying]($tfe),
                tc($aExpr, ${ lhsExactInfo.exactExpr })(using dfc)
              )(using dfc)
                // TODO: `P` should be automatically derived from tc.OutP, but there is issue
                // https://github.com/lampepfl/dotty/issues/19554
                .asValTP[DFOpaque[tfeType.Underlying], pType.Underlying]
            }
          case _ =>
            report.errorAndAbort("Not a valid opaque type companion.")
        end match
      end asMacro

      extension [AT <: DFTypeAny, TFE <: Frontend[AT], A, P](
          lhs: DFVal[DFOpaque[TFE], Modifier[A, Any, Any, P]]
      )
        def opaqueType: TFE = lhs.dfType.asIR.id.asInstanceOf[TFE]
        def actual(using DFC): DFVal[AT, Modifier[A, Any, Any, P]] = trydf {
          DFVal.Alias.AsIs(lhs.dfType.actualType, lhs)
        }
        def mapActual(
            f: DFValOf[AT] => DFValOf[AT]
        )(using dfc: DFC, ce: ClassEv[TFE]): DFValOf[DFOpaque[TFE]] =
          DFVal.Alias.AsIs(
            DFOpaque[TFE](ce.value),
            f(lhs.actual)
          )
      end extension
    end Ops
  end Val

end DFOpaque
