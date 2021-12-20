package DFiant.core
import DFiant.compiler.printing.Printer
import DFiant.compiler.ir
import ir.DFVal.Func.Op as FuncOp
import DFiant.internals.*

import scala.quoted.*
import scala.annotation.implicitNotFound
import scala.annotation.unchecked.uncheckedVariance

final class DFToken[+T <: DFTypeAny](val value: ir.DFTokenAny)
    extends AnyVal
    with Selectable:
//  transparent inline def selectDynamic(inline name: String): Any = ${
//    selectMacro('this, 'name)
//  }

  def selectDynamic(name: String): Any =
    val ir.DFStruct(structName, fieldMap) = value.dfType
//    val dfType = fieldMap(name)
    1
  transparent inline def ==[R](
      inline that: R
  )(using DFC): DFBool <> TOKEN = ${
    DFToken.equalityMacro[T, R, FuncOp.===.type]('this, 'that)
  }
  transparent inline def !=[R](
      inline that: R
  )(using DFC): DFBool <> TOKEN = ${
    DFToken.equalityMacro[T, R, FuncOp.=!=.type]('this, 'that)
  }
  def ==[R <: DFTypeAny](that: DFValOf[R])(using
      dfc: DFC,
      op: DFVal.Compare[R, DFToken[T], FuncOp.===.type, true]
  ): DFBool <> VAL = op(that, this)
  def !=[R <: DFTypeAny](that: DFValOf[R])(using
      dfc: DFC,
      op: DFVal.Compare[R, DFToken[T], FuncOp.=!=.type, true]
  ): DFBool <> VAL = op(that, this)
  override def toString: String = value.toString
end DFToken

type DFTokenAny = DFToken[DFTypeAny]
extension (tokenIR: ir.DFTokenAny)
  def asTokenOf[T <: DFTypeAny]: DFToken[T] = DFToken[T](tokenIR)

object DFToken:
//  trait Refiner[T <: DFFields]:
//    type Out <: DFToken[DFStruct[T]]
//  object Refiner:
//    transparent inline given [T <: DFFields]: Refiner[T] = ${
//      refineMacro[T]
//    }
//    def refineMacro[T <: DFFields](using
//        Quotes,
//        Type[T]
//    ): Expr[Refiner[T]] =
//      import quotes.reflect.*
//      val tpt = TypeRepr.of[DFToken[DFStruct[T]]].asTypeTree
//      val sym = Symbol.newVal(
//        Symbol.noSymbol,
//        "bash",
//        TypeRepr.of[Int],
//        Flags.EmptyFlags,
//        Symbol.noSymbol
//      )
//      val r =
//        Refined
//          .copy(tpt)(tpt, List(ValDef(sym, None)))
//      println(r.tpe.show)
//      '{
//        new Refiner[T]:
//          type Out = DFToken[DFStruct[T]] {
//            val bash: Int
//          }
//      }
//    end refineMacro
//  end Refiner
//  def selectMacro[T <: DFTypeAny](
//      token: Expr[DFToken[T]],
//      name: Expr[String]
//  )(using Quotes, Type[T]): Expr[Any] =
//    import quotes.reflect.*
//    Type.of[T] match
//      case '[DFTuple[t]] =>
//      case '[DFStruct[t]] =>

//  implicit def refined[T <: DFFields](token: DFToken[DFStruct[T]])(using
//      r: Refiner[T]
//  ): r.Out = token.asInstanceOf[r.Out]

  def equalityMacro[T <: DFTypeAny, R, Op <: FuncOp](
      token: Expr[DFToken[T]],
      arg: Expr[R]
  )(using Quotes, Type[T], Type[R], Type[Op]): Expr[DFToken[DFBool]] =
    import quotes.reflect.*
    val exact = arg.asTerm.exactTerm
    val exactExpr = exact.asExpr
    val exactType = exact.tpe.asTypeOf[Any]
    '{
      val c = compiletime.summonInline[
        DFToken.Compare[T, exactType.Underlying, Op, false]
      ]
      c($token, $exactExpr)(using
        compiletime.summonInline[ValueOf[Op]],
        new ValueOf[false](false)
      )
    }
  end equalityMacro

  // Implicit conversions for tokens
  implicit inline def fromTC[T <: DFTypeAny, V](
      inline value: V
  )(using es: Exact.Summon[V, value.type])(using
      dfType: T,
      tc: DFToken.TC[T, es.Out]
  ): DFToken[T] = tc(dfType, es(value))

  // Enabling equality with Int, Boolean, and Tuples.
  // just to give a better error message via the compiler plugin.
  // See the method `rejectBadPrimitiveOps` in `MetaContextGenPhase.scala`
  given [T <: DFTypeAny]: CanEqual[Int, DFToken[T]] =
    CanEqual.derived
  given [T <: DFTypeAny]: CanEqual[Boolean, DFToken[T]] =
    CanEqual.derived
  given [T <: DFTypeAny]: CanEqual[Tuple, DFToken[T]] =
    CanEqual.derived

  protected[core] def bubble[T <: DFTypeAny](dfType: T): DFToken[T] =
    ir.DFToken.bubble(dfType.asIR).asTokenOf[T]
  extension (token: DFTokenAny)
    def asIR: ir.DFTokenAny = token.value
    def codeString(using printer: Printer): String = printer.csDFToken(asIR)
  extension [T <: ir.DFType, Data](
      token: DFToken[DFType[ir.DFType.Aux[T, Data], Args]]
  ) def data: Data = token.value.data.asInstanceOf[Data]

  @implicitNotFound("Unsupported token value ${V} for dataflow type ${T}")
  trait TC[T <: DFTypeAny, -V] extends TCConv[T, V, DFTokenAny]:
    type Out = DFToken[T]
    def apply(dfType: T, value: V): Out = conv(dfType, value)

  trait TCLPLP:
    transparent inline given errorDMZ[T <: DFTypeAny, R](using
        t: ShowType[T],
        r: ShowType[R]
    ): TC[T, R] =
      Error.call[
        (
            "Unsupported value of type `",
            r.Out,
            "` for dataflow receiver type `",
            t.Out,
            "`."
        )
      ]
    inline given sameTokenType[T <: DFTypeAny]: TC[T, T <> TOKEN] with
      def conv(dfType: T, value: T <> TOKEN): Out =
        assert(dfType == value.dfType)
        value
  end TCLPLP
  trait TCLP extends TCLPLP
  object TC extends TCLP:
    export DFBoolOrBit.Token.TC.given
    export DFBits.Token.TC.given
    export DFDecimal.Token.TC.given
    export DFEnum.Token.TC.given
    export DFTuple.Token.TC.given

    transparent inline given DFTokenFromBubble[T <: DFTypeAny]: TC[T, Bubble] =
      (dfType: T, value: Bubble) => Bubble(dfType)
  end TC

  @implicitNotFound("Cannot compare token of ${T} with value of ${V}")
  trait Compare[T <: DFTypeAny, -V, Op <: FuncOp, C <: Boolean]
      extends TCConv[T, V, DFTokenAny]:
    type Out = DFToken[T]
    def apply(token: DFToken[T], arg: V)(using
        op: ValueOf[Op],
        castling: ValueOf[C]
    ): DFToken[DFBool] =
      given CanEqual[Any, Any] = CanEqual.derived
      val tokenArg = conv(token.dfType, arg)
      assert(token.dfType == tokenArg.dfType)
      val dataOut = op.value match
        case FuncOp.=== => token.asIR.data == tokenArg.asIR.data
        case FuncOp.=!= => token.asIR.data != tokenArg.asIR.data
        case _          => throw new IllegalArgumentException("Unsupported Op")
      DFBoolOrBit.Token(DFBool, dataOut)
    def conv(dfType: T, arg: V): DFToken[T]
  end Compare

  trait CompareLPLP:
    transparent inline given errorDMZ[
        T <: DFTypeAny,
        R,
        Op <: FuncOp,
        C <: Boolean
    ](using
        t: ShowType[T],
        r: ShowType[R]
    ): Compare[T, R, Op, C] =
      Error.call[
        (
            "Cannot compare token of type `",
            t.Out,
            "` with value of type `",
            r.Out,
            "`."
        )
      ]
    inline given sameTokenType[T <: DFTypeAny, Op <: FuncOp, C <: Boolean](using
        op: ValueOf[Op]
    ): Compare[T, T <> TOKEN, Op, C] with
      def conv(dfType: T, arg: T <> TOKEN): DFToken[T] = arg
  end CompareLPLP
  trait CompareLP extends CompareLPLP
  object Compare extends CompareLP:
    export DFBoolOrBit.Token.Compare.given
    export DFBits.Token.Compare.given
    export DFDecimal.Token.Compare.given
    export DFEnum.Token.Compare.given
    export DFTuple.Token.Compare.given

  object Ops:
    extension [T <: DFTypeAny](token: DFToken[T])
      def bits(using w: Width[T]): DFToken[DFBits[w.Out]] =
        import ir.DFToken.bits as bitsIR
        token.asIR.bitsIR.asTokenOf[DFBits[w.Out]]
  end Ops

  trait Value[T <: DFTypeAny]:
    type Out <: DFTokenAny
    def apply(dfType: T): Out
  object Value:
    transparent inline implicit def fromValue[T <: DFTypeAny, V](
        inline value: V
    ): Value[T] = ${ fromValueMacro[T, V]('value) }

    def fromValueMacro[T <: DFTypeAny, V](
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
  trait TupleValues[T <: NonEmptyTuple]:
    def apply(dfType: DFTuple[T]): List[DFTokenAny]
  object TupleValues:
    transparent inline implicit def fromValue[T <: NonEmptyTuple, V](
        inline value: V
    ): TupleValues[T] = ${ fromValueMacro[T, V]('value) }

    def fromValueMacro[T <: NonEmptyTuple, V](
        value: Expr[V]
    )(using Quotes, Type[T], Type[V]): Expr[TupleValues[T]] =
      import quotes.reflect.*
      val term = value.asTerm.underlyingArgument
      val tTpe = TypeRepr.of[T]
      val vTpe = term.tpe
      val multiElements = vTpe.asTypeOf[Any] match
        case '[NonEmptyTuple] =>
          vTpe.getTupleArgs.forall(va => tTpe.tupleSigMatch(va, false))
        case _ => false
      // In the case we have a multiple elements in the tuple value that match the signature
      // of the dataflow type, then each element is considered as a candidate
      if (multiElements)
        val Apply(_, vArgsTerm) = term
        def tokens(dfType: Expr[DFTuple[T]]): List[Expr[DFTokenAny]] =
          vArgsTerm.map { a =>
            val aTerm = a.exactTerm
            val aType = aTerm.tpe.asTypeOf[Any]
            '{
              val tc =
                compiletime
                  .summonInline[DFToken.TC[DFTuple[T], aType.Underlying]]
              tc($dfType, ${ aTerm.asExpr })
            }
          }
        '{
          new TupleValues[T]:
            def apply(dfType: DFTuple[T]): List[DFTokenAny] =
              List(${ Expr.ofList(tokens('{ dfType })) }*)
        }
      // otherwise the entire tuple is considered the token candidate.
      else
        val vTerm = term.exactTerm
        val vType = vTerm.tpe.asTypeOf[Any]
        '{
          val tc = compiletime
            .summonInline[DFToken.TC[DFTuple[T], vType.Underlying]]
          new TupleValues[T]:
            def apply(dfType: DFTuple[T]): List[DFTokenAny] =
              List(tc(dfType, ${ vTerm.asExpr }))
        }
      end if
    end fromValueMacro
  end TupleValues
end DFToken
