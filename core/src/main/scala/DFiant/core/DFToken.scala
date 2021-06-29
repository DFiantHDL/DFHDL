package DFiant.core
import DFiant.compiler.printing.Printer
import DFiant.compiler.ir
import DFiant.internals.*
import scala.quoted.*
import scala.annotation.implicitNotFound

opaque type DFToken = ir.DFType.Token
object DFToken:
  extension (of: DFToken)
    def asIR: ir.DFType.Token = of
    def codeString(using printer: Printer): String = printer.csDFToken(asIR)

  opaque type Of[+T <: DFType, D] <: DFToken = DFToken
  object Of:
    extension [T <: DFType, D](token: Of[T, D])
      def data: D = token.asIR.data.asInstanceOf[D]
      def dfType: T = token.asIR.dfType.asInstanceOf[T]
      def width(using w: Width[T]): Inlined.Int[w.Out] =
        Inlined.Int.forced[w.Out](token.asIR.width)
  @implicitNotFound("Unsupported token value ${V} for type ${T}")
  trait TC[T <: DFType, V]:
    type Out <: DFToken
    def apply(dfType: T, value: V): Out
  object TC:
    transparent inline given DFBoolTokenFromBubble[V <: Bubble]
        : TC[DFBoolOrBit, V] =
      new TC[DFBoolOrBit, V]:
        type Out = DFBoolOrBit.Token
        def apply(dfType: DFBoolOrBit, value: V): Out =
          DFBoolOrBit.Token(dfType, value)
    transparent inline given DFBoolTokenFromBooleanSing[V <: Boolean]
        : TC[DFBoolOrBit, ValueOf[V]] =
      new TC[DFBoolOrBit, ValueOf[V]]:
        type Out = DFBoolOrBit.Token
        def apply(dfType: DFBoolOrBit, value: ValueOf[V]): Out =
          DFBoolOrBit.Token(dfType, value.value)
    transparent inline given DFBoolTokenFromBoolean[V <: Boolean]
        : TC[DFBoolOrBit, V] =
      new TC[DFBoolOrBit, V]:
        type Out = DFBoolOrBit.Token
        def apply(dfType: DFBoolOrBit, value: V): Out =
          DFBoolOrBit.Token(dfType, value)
    transparent inline given DFBoolTokenFrom1Or0[V <: 0 | 1]
        : TC[DFBoolOrBit, ValueOf[V]] =
      new TC[DFBoolOrBit, ValueOf[V]]:
        type Out = DFBoolOrBit.Token
        def apply(dfType: DFBoolOrBit, value: ValueOf[V]): Out =
          DFBoolOrBit.Token(dfType, value.value)
    transparent inline given DFBoolTokenFromToken[V <: DFBoolOrBit.Token]
        : TC[DFBoolOrBit, V] =
      new TC[DFBoolOrBit, V]:
        type Out = DFBoolOrBit.Token
        def apply(dfType: DFBoolOrBit, value: V): Out =
          DFBoolOrBit.Token(dfType, value.data)

    protected object `W == VW`
        extends Check2[
          Int,
          Int,
          [W <: Int, VW <: Int] =>> W == VW,
          [W <: Int, VW <: Int] =>> "The token width (" +
            ToString[VW] +
            ") is different than the DFType width (" +
            ToString[W] +
            ")."
        ]
    transparent inline given DFBitsTokenFromBubble[W <: Int, V <: Bubble]
        : TC[DFBits[W], V] =
      new TC[DFBits[W], V]:
        type Out = DFBits.Token[W]
        def apply(dfType: DFBits[W], value: V): Out =
          DFBits.Token[W](dfType.width, value)

    transparent inline given DFBitsTokenFromToken[W <: Int, VW <: Int](using
        check: `W == VW`.Check[W, VW]
    ): TC[DFBits[W], DFBits.Token[VW]] = new TC[DFBits[W], DFBits.Token[VW]]:
      type Out = DFBits.Token[W]
      def apply(dfType: DFBits[W], value: DFBits.Token[VW]): Out =
        check(dfType.width, value.width)
        DFBits.Token[W](dfType, value.data)

    transparent inline given DFBitsTokenFromSBV[W <: Int, V <: SameBitsVector]
        : TC[DFBits[W], V] = new TC[DFBits[W], V]:
      type Out = DFBits.Token[W]
      def apply(dfType: DFBits[W], value: V): Out =
        DFBits.Token[W](dfType.width, value)

    transparent inline given DFTupleTokenFromTuple[T, V <: NonEmptyTuple](using
        creator: DFTuple.Token.Creator[T, V]
    ): TC[DFTuple[T], V] = new TC[DFTuple[T], V]:
      type Out = DFTuple.Token[T]
      def apply(dfType: DFTuple[T], value: V): Out =
        DFTuple.Token[T](dfType, creator(dfType.fieldList, value))
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
      def exactTerm(term: Term): Term =
        term match
          case Literal(const) =>
            val constTpe = ConstantType(const).asType.asInstanceOf[Type[Any]]
            val expr =
              '{
                ValueOf[constTpe.Underlying](${ term.asExpr })
              }
            expr.asTerm
          case Apply(TypeApply(fun, _), tupleArgs)
              if term.tpe <:< TypeRepr.of[NonEmptyTuple] =>
            val terms = tupleArgs.map(exactTerm)
            val tpes = terms.map(_.tpe)
            val AppliedType(tycon, _) = term.tpe
            val tupleTypeArgs = tpes.map { t =>
              t.asType match
                case '[t] =>
                  TypeTree.of[t]
            }
            Apply(TypeApply(fun, tupleTypeArgs), terms)
          case _ =>
            term
      val term = exactTerm(value.asTerm.underlyingArgument)
      val tpe = term.tpe.asType.asInstanceOf[Type[Any]]
      '{
        val tc = compiletime.summonInline[DFToken.TC[T, tpe.Underlying]]
        new Value[T]:
          type Out = tc.Out
          def apply(dfType: T): Out =
            tc(dfType, ${ term.asExpr })
      }
    end fromValueMacro
