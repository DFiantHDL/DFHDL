package dfhdl.core
import dfhdl.compiler.printing.Printer
import dfhdl.compiler.ir
import ir.DFVal.Func.Op as FuncOp
import dfhdl.internals.*

import scala.quoted.*
import scala.annotation.implicitNotFound
import scala.annotation.unchecked.uncheckedVariance

final class DFToken[+T <: DFTypeAny](val value: ir.DFTokenAny | DFError)
    extends AnyVal
    with Selectable:

  def selectDynamic(name: String): Any =
    val ir.DFStruct(structName, fieldMap) = this.asIR.dfType: @unchecked
    val dfType = fieldMap(name)
    val idx = fieldMap.toList.indexWhere(_._1 == name)
    val data = this.asIR.data.asInstanceOf[List[Any]](idx)
    ir.DFToken.forced(dfType, data).asTokenOf[DFTypeAny]

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
  override def toString: String = value.toString
end DFToken

type DFTokenAny = DFToken[DFTypeAny]
extension (tokenIR: ir.DFTokenAny)
  def asTokenOf[T <: DFTypeAny]: DFToken[T] = DFToken[T](tokenIR)
  def asTokenAny: DFTokenAny = DFToken[DFTypeAny](tokenIR)
extension (token: DFTokenAny)
  def asTokenOf[T <: DFTypeAny]: DFToken[T] = token.asInstanceOf[DFToken[T]]

object DFToken:
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
      c($token, $exactExpr)(using compiletime.summonInline[ValueOf[Op]], new ValueOf[false](false))
    }
  end equalityMacro

  // Implicit conversions for tokens
  implicit inline def fromTC[T <: DFTypeAny, V](
      value: V
  )(using es: Exact.Summon[V, value.type])(using
      dfType: T,
      tc: DFToken.TC[T, es.Out]
  ): DFToken[T] = tc(dfType, es(value))
  // implicit conversion to allow a boolean/bit token to be a Scala Boolean
  implicit def fromDFBoolOrBitToken(from: DFToken[DFBoolOrBit]): Boolean =
    from.asIR.data.asInstanceOf[ir.DFBool.Data].get

  // Enabling equality with Int, Boolean, and Tuples.
  // just to give a better error message via the compiler plugin.
  // See the method `rejectBadPrimitiveOps` in `MetaContextGenPhase.scala`
  given [T <: DFTypeAny]: CanEqual[Int, DFToken[T]] =
    CanEqual.derived
  given [T <: DFTypeAny]: CanEqual[Boolean, DFToken[T]] =
    CanEqual.derived
  given [T <: DFTypeAny]: CanEqual[Tuple, DFToken[T]] =
    CanEqual.derived

  given __refined_token[T <: FieldsOrTuple](using
      r: DFStruct.Token.Refiner[T]
  ): Conversion[DFToken[DFStruct[T]], r.Out] = _.asInstanceOf[r.Out]

  protected[core] def bubble[T <: DFTypeAny](dfType: T): DFToken[T] =
    ir.DFToken.bubble(dfType.asIR).asTokenOf[T]
  extension (token: DFTokenAny)
    def asIR: ir.DFTokenAny = token.value match
      case tokenIR: ir.DFTokenAny => tokenIR
      case err: DFError           => throw DFError.Derived(err)
    def codeString(using printer: Printer)(using DFC): String =
      printer.csDFToken(asIR)
  extension [T <: ir.DFType, Data](
      token: DFToken[DFType[ir.DFType.Aux[T, Data], Args]]
  ) def data: Data = token.asIR.data.asInstanceOf[Data]

  @implicitNotFound("Unsupported token value ${V} for DFHDL type ${T}")
  trait TC[T <: DFTypeAny, V] extends TCConv[T, V, DFTokenAny]:
    type Out = DFToken[T]
    type Ctx = DummyImplicit
    final def apply(dfType: T, value: V): Out = conv(dfType, value)

  trait TCLPLP:
    transparent inline given errorDMZ[T <: DFTypeAny, R](using
        t: ShowType[T],
        r: ShowType[R]
    ): TC[T, R] =
      Error.call[
        (
            "Unsupported value of type `",
            r.Out,
            "` for DFHDL receiver type `",
            t.Out,
            "`."
        )
      ]
    inline given sameTokenType[T <: DFTypeAny, V <: T <> TOKEN]: TC[T, V] with
      def conv(dfType: T, value: V)(using Ctx): Out =
        require(dfType == value.dfType)
        value
  end TCLPLP
  trait TCLP extends TCLPLP
  object TC extends TCLP:
    export DFBoolOrBit.Token.TC.given
    export DFBits.Token.TC.given
    export DFDecimal.Token.TC.given
    export DFEnum.Token.TC.given
    export DFVector.Token.TC.given
    export DFTuple.Token.TC.given
    export DFStruct.Token.TC.given
    export DFOpaque.Token.TC.given

    given DFTokenFromBubble[T <: DFTypeAny, V <: Bubble]: TC[T, V] with
      def conv(dfType: T, value: V)(using Ctx): DFToken[T] = Bubble(dfType)
  end TC

  @implicitNotFound("Cannot compare token of ${T} with value of ${V}")
  trait Compare[T <: DFTypeAny, V, Op <: FuncOp, C <: Boolean] extends TCConv[T, V, DFTokenAny]:
    type Out = DFToken[T]
    type Ctx = DummyImplicit
    def apply(token: DFToken[T], arg: V)(using
        op: ValueOf[Op],
        castling: ValueOf[C]
    ): DFToken[DFBool] =
      given CanEqual[Any, Any] = CanEqual.derived
      val tokenArg = conv(token.dfType, arg)
      require(token.dfType == tokenArg.dfType)
      val dataOut = op.value match
        case FuncOp.=== => token.asIR.data == tokenArg.asIR.data
        case FuncOp.=!= => token.asIR.data != tokenArg.asIR.data
        case _          => throw new IllegalArgumentException("Unsupported Op")
      DFBoolOrBit.Token(DFBool, dataOut)
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
    inline given sameTokenType[T <: DFTypeAny, V <: T <> TOKEN, Op <: FuncOp, C <: Boolean](using
        op: ValueOf[Op]
    ): Compare[T, V, Op, C] with
      def conv(dfType: T, arg: V)(using Ctx): DFToken[T] = arg
  end CompareLPLP
  trait CompareLP extends CompareLPLP
  object Compare extends CompareLP:
    export DFBoolOrBit.Token.Compare.given
    export DFBits.Token.Compare.given
    export DFDecimal.Token.Compare.given
    export DFEnum.Token.Compare.given
    export DFVector.Token.Compare.given
    export DFTuple.Token.Compare.given

  object Ops:
    extension [T <: DFTypeAny](token: DFToken[T])
      def bits(using w: Width[T]): DFToken[DFBits[w.Out]] =
        import ir.DFToken.bits as bitsIR
        token.asIR.bitsIR.asTokenOf[DFBits[w.Out]]
  end Ops

end DFToken
