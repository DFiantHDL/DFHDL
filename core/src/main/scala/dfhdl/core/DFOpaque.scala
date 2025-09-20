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
  protected[core] sealed trait Abstract extends HasTypeName, Product, Serializable:
    type ActualType <: DFTypeAny
    protected[core] val actualType: ActualType
  object Abstract:
    type Aux[TFE <: Abstract, AT <: DFTypeAny] = TFE { type ActualType = AT }
    inline implicit def fromComp[TFE <: Abstract, Comp <: Object](tfeComp: Comp)(implicit
        cc: CaseClass[Comp, TFE]
    ): cc.CC = compiletime.summonInline[ClassEv[cc.CC]].value
    def fromCompMacro[TFE <: Abstract, Comp <: Object](using
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

  abstract class Magnet[A <: DFTypeAny](actualType: A) extends Frontend[A](actualType)
  abstract class Clk extends Magnet[DFBit](DFBit)
  abstract class Rst extends Magnet[DFBit](DFBit)

  given [TFE <: Abstract](using ce: ClassEv[TFE], dfc: DFC): DFOpaque[TFE] = DFOpaque(ce.value)

  def apply[TFE <: Abstract](
      t: TFE
  )(using DFC): DFOpaque[TFE] = trydf:
    val kind = t match
      case _: Clk       => ir.DFOpaque.Kind.Clk
      case _: Rst       => ir.DFOpaque.Kind.Rst
      case _: Magnet[?] => ir.DFOpaque.Kind.Magnet
      case _            => ir.DFOpaque.Kind.General
    // Generate a stable ID based on the fully qualified class name
    // This ensures different case classes have different IDs even if they have the same simple name
    // but are in different packages, and remains stable between runs
    val fullyQualifiedClassName = t.getClass.getName
    val id = fullyQualifiedClassName.hashCode
    ir.DFOpaque(
      t.typeName,
      kind,
      id,
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
      given evOpAsDFOpaqueComp[
          L,
          Comp <: Object,
          TFE <: Abstract
      ](using
          cc: CaseClass.Aux[Comp, Abstract, TFE]
      )(using
          ce: ClassEv[TFE]
      )(using
          tc: DFVal.TC[ce.value.ActualType, L]
      ): ExactOp2Aux["as", DFC, DFValAny, L, Comp, DFValTP[DFOpaque[TFE], tc.OutP]] =
        new ExactOp2["as", DFC, DFValAny, L, Comp]:
          type Out = DFValTP[DFOpaque[TFE], tc.OutP]
          def apply(lhs: L, tfeComp: Comp)(using DFC): Out = trydf {
            DFVal.Alias.AsIs(DFOpaque[TFE](ce.value), tc(ce.value.actualType, lhs))
          }(using dfc, CTName("cast as opaque"))
      end evOpAsDFOpaqueComp

      given evOpAsDFOpaqueTFE[
          L,
          T <: DFTypeAny,
          TFE <: Frontend[T]
      ](using
          tc: DFVal.TC[T, L]
      ): ExactOp2Aux["as", DFC, DFValAny, L, TFE, DFValTP[DFOpaque[TFE], tc.OutP]] =
        new ExactOp2["as", DFC, DFValAny, L, TFE]:
          type Out = DFValTP[DFOpaque[TFE], tc.OutP]
          def apply(lhs: L, tfe: TFE)(using DFC): Out = trydf {
            DFVal.Alias.AsIs(DFOpaque[TFE](tfe), tc(tfe.actualType, lhs))
          }(using dfc, CTName("cast as opaque"))
      end evOpAsDFOpaqueTFE

      given evOpAsDFOpaqueIterable[
          A <: DFTypeAny,
          P,
          L <: Iterable[DFValTP[A, P]],
          T <: DFTypeAny,
          TFE <: Frontend[T]
      ](using
          tc: DFVal.TC[T, DFValTP[DFVector[A, Tuple1[Int]], P]]
      ): ExactOp2Aux["as", DFC, DFValAny, L, TFE, DFValTP[DFOpaque[TFE], tc.OutP]] =
        new ExactOp2["as", DFC, DFValAny, L, TFE]:
          type Out = DFValTP[DFOpaque[TFE], tc.OutP]
          def apply(lhs: L, tfe: TFE)(using DFC): Out = trydf {
            DFVal.Alias.AsIs(DFOpaque[TFE](tfe), tc(tfe.actualType, asDFVector(lhs)))
          }(using dfc, CTName("cast as opaque"))
      end evOpAsDFOpaqueIterable

      private def asDFVector[A <: DFTypeAny, P](dfVals: Iterable[DFValTP[A, P]])(using
          DFC
      ): DFValTP[DFVector[A, Tuple1[Int]], P] =
        val dfType = DFVector[A, Tuple1[Int]](dfVals.head.dfType, List(dfVals.size))
        DFVal.Func(dfType, DFVal.Func.Op.++, dfVals.toList)

      extension [AT <: DFTypeAny, TFE <: Frontend[AT], A, P](
          lhs: DFVal[DFOpaque[TFE], Modifier[A, Any, Any, P]]
      )
        def opaqueType(using ce: ClassEv[TFE]): TFE = ce.value
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
