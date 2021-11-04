package DFiant.core
import DFiant.compiler.ir
import DFiant.compiler.printing.{DefaultPrinter, Printer}
import DFiant.internals.*

import scala.annotation.unchecked.uncheckedVariance

type DFOpaque[+T <: DFOpaque.Abstract] =
  DFType[ir.DFOpaque, Args1[T @uncheckedVariance]]
object DFOpaque:
  protected[core] sealed trait Abstract extends HasTypeName:
    type ActualType <: DFTypeAny
    val actualType: ActualType
  class Frontend[T <: DFTypeAny](final val actualType: T) extends Abstract:
    type ActualType = T

  def apply[T <: Abstract](
      t: T
  ): DFOpaque[T] =
    ir.DFOpaque(t.typeName, t.actualType.asIR).asFE[DFOpaque[T]]
  extension [T <: DFTypeAny, TFE <: Frontend[T]](dfType: DFOpaque[TFE])
    def actualType: T = dfType.asIR.actualType.asFE[T]

  private def checkAs[T <: DFTypeAny](tokenDFType: T, actualDFType: T): Unit =
    given Printer = DefaultPrinter
    assert(
      tokenDFType == actualDFType,
      s"The token type (${tokenDFType.codeString}) does not match the actual opaque type ${actualDFType.codeString}"
    )

  type Token[T <: Abstract] = DFToken[DFOpaque[T]]
  object Token:
    def apply[T <: DFTypeAny, TFE <: Frontend[T]](
        tfe: TFE,
        token: T <> TOKEN
    ): Token[TFE] =
      ir.DFToken(DFOpaque(tfe).asIR, token.asIR).asTokenOf[DFOpaque[TFE]]

    object Ops:
      extension [T <: DFTypeAny](lhs: T <> TOKEN)
        def as[TFE <: Frontend[T]](tfe: TFE): DFOpaque[TFE] <> TOKEN =
          checkAs(lhs.dfType, tfe.actualType)
          Token(tfe, lhs)
      extension [T <: DFTypeAny, TFE <: Frontend[T]](
          lhs: DFOpaque[TFE] <> TOKEN
      )
        def actual: T <> TOKEN =
          lhs.asIR.data.asInstanceOf[ir.DFTokenAny].asTokenOf[T]
  end Token

  object Val:
    object Ops:
      import ir.DFVal.Modifier
      extension [T <: DFTypeAny, M <: Modifier](lhs: DFVal[T, M])
        def as[TFE <: Frontend[T]](tfe: TFE)(using
            DFC
        ): DFVal[DFOpaque[TFE], M] =
          import Token.Ops.{as => asToken}
          checkAs(lhs.dfType, tfe.actualType)
          DFVal.Alias.AsIs(DFOpaque(tfe), lhs, _.asToken(tfe))
      extension [T <: DFTypeAny, TFE <: Frontend[T], M <: Modifier](
          lhs: DFVal[DFOpaque[TFE], M]
      )
        def actual(using DFC): DFVal[T, M] =
          import Token.Ops.{actual => actualToken}
          DFVal.Alias.AsIs(lhs.dfType.actualType, lhs, _.actualToken)
    end Ops
  end Val

end DFOpaque
