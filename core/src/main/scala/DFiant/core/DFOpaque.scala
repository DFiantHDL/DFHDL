package DFiant.core
import DFiant.compiler.ir
import DFiant.compiler.printing.{DefaultPrinter, Printer}
import DFiant.internals.*

import scala.annotation.unchecked.uncheckedVariance

type DFOpaque[+Id, T <: DFTypeAny] =
  DFType[ir.DFOpaque, Args2[Id @uncheckedVariance, T]]
object DFOpaque:
  class Frontend[T <: DFTypeAny](final val actualType: T) extends HasTypeName
  object Frontend:
    def apply[T <: DFTypeAny](actualType: T)(using
        name: CTName,
        uniqueId: UniqueId
    ): DFOpaque[(name.Out, uniqueId.Out), T] =
      DFOpaque[(name.Out, uniqueId.Out), T](name.value, actualType)

  def apply[Id, T <: DFTypeAny](
      name: String,
      actualType: T
  ): DFOpaque[Id, T] =
    ir.DFOpaque(name, actualType.asIR).asFE[DFOpaque[Id, T]]
  extension [Id, T <: DFTypeAny](dfType: DFOpaque[Id, T])
    def actualType: T = dfType.asIR.actualType.asFE[T]

  private def checkAs[T <: DFTypeAny](tokenDFType: T, actualDFType: T): Unit =
    given Printer = DefaultPrinter
    assert(
      tokenDFType == actualDFType,
      s"The token type (${tokenDFType.codeString}) does not match the actual opaque type ${actualDFType.codeString}"
    )

  type Token[Id, T <: DFTypeAny] = DFToken[DFOpaque[Id, T]]
  object Token:
    def apply[Id, T <: DFTypeAny](
        dfType: DFOpaque[Id, T],
        token: T <> TOKEN
    ): Token[Id, T] =
      ir.DFToken(dfType.asIR, token.asIR).asTokenOf[DFOpaque[Id, T]]

    object Ops:
      extension [T <: DFTypeAny](lhs: T <> TOKEN)
        def as[Id](opaque: DFOpaque[Id, T]): DFOpaque[Id, T] <> TOKEN =
          checkAs(lhs.dfType, opaque.actualType)
          Token(opaque, lhs)
      extension [Id, T <: DFTypeAny](lhs: DFOpaque[Id, T] <> TOKEN)
        def actual: T <> TOKEN =
          lhs.asIR.data.asInstanceOf[ir.DFTokenAny].asTokenOf[T]
  end Token

  object Val:
    object Ops:
      import ir.DFVal.Modifier
      extension [T <: DFTypeAny, M <: Modifier](lhs: DFVal[T, M])
        def as[Id](opaque: DFOpaque[Id, T])(using
            DFC
        ): DFVal[DFOpaque[Id, T], M] =
          import Token.Ops.{as => asToken}
          checkAs(lhs.dfType, opaque.actualType)
          DFVal.Alias.AsIs(opaque, lhs, _.asToken(opaque))
      extension [Id, T <: DFTypeAny, M <: Modifier](
          lhs: DFVal[DFOpaque[Id, T], M]
      )
        def actual(using DFC): DFVal[T, M] =
          import Token.Ops.{actual => actualToken}
          DFVal.Alias.AsIs(lhs.dfType.actualType, lhs, _.actualToken)
    end Ops
  end Val

end DFOpaque
