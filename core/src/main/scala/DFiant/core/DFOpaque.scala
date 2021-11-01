package DFiant.core
import DFiant.compiler.ir
import DFiant.compiler.printing.{DefaultPrinter, Printer}
import DFiant.internals.*

import scala.annotation.unchecked.uncheckedVariance

type DFOpaque[Id <: Int, T <: DFTypeAny] =
  DFType[ir.DFOpaque, Args2[Id @uncheckedVariance, T]]
object DFOpaque:
  def apply[Id <: Int, T <: DFTypeAny](
      name: String,
      actualType: T
  ): DFOpaque[Id, T] =
    ir.DFOpaque(name, actualType.asIR).asFE[DFOpaque[Id, T]]
  extension [Id <: Int, T <: DFTypeAny](dfType: DFOpaque[Id, T])
    def actualType: T = dfType.asIR.actualType.asFE[T]

  object Ops:
    extension [T <: DFType.Supported](t: T)(using tc: DFType.TC[T])
      def opaque(using
          name: CTName,
          uniqueId: UniqueId
      ): DFOpaque[uniqueId.Out, tc.Type] =
        DFOpaque[uniqueId.Out, tc.Type](name.value, tc(t))

  private def checkAs[T <: DFTypeAny](tokenDFType: T, actualDFType: T): Unit =
    given Printer = DefaultPrinter
    assert(
      tokenDFType == actualDFType,
      s"The token type (${tokenDFType.codeString}) does not match the actual opaque type ${actualDFType.codeString}"
    )

  type Token[Id <: Int, T <: DFTypeAny] = DFToken[DFOpaque[Id, T]]
  object Token:
    def apply[Id <: Int, T <: DFTypeAny](
        dfType: DFOpaque[Id, T],
        token: T <> TOKEN
    ): Token[Id, T] =
      ir.DFToken(dfType.asIR, token.asIR).asTokenOf[DFOpaque[Id, T]]

    object Ops:
      extension [T <: DFTypeAny](lhs: T <> TOKEN)
        def as[Id <: Int](opaque: DFOpaque[Id, T]): DFOpaque[Id, T] <> TOKEN =
          checkAs(lhs.dfType, opaque.actualType)
          Token(opaque, lhs)
  end Token

  object Val:
    object Ops:
      extension [T <: DFTypeAny](lhs: T <> VAL)
        def as[Id <: Int](opaque: DFOpaque[Id, T]): DFOpaque[Id, T] <> VAL =
          checkAs(lhs.dfType, opaque.actualType)
          ???

end DFOpaque
