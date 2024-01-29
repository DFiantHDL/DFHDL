package dfhdl.core
import dfhdl.compiler.printing.Printer
import dfhdl.compiler.ir
import ir.DFVal.Func.Op as FuncOp
import dfhdl.internals.*

import scala.quoted.*
import scala.annotation.implicitNotFound
import scala.annotation.unchecked.uncheckedVariance

final class DFToken[+T <: DFTypeAny](val value: ir.DFTokenAny | DFError) extends AnyVal:
  override def toString: String = value.toString
end DFToken

type DFTokenAny = DFToken[DFTypeAny]
extension (tokenIR: ir.DFTokenAny)
  def asTokenOf[T <: DFTypeAny]: DFToken[T] = DFToken[T](tokenIR)
  def asTokenAny: DFTokenAny = DFToken[DFTypeAny](tokenIR)
extension (token: DFTokenAny)
  def asTokenOf[T <: DFTypeAny]: DFToken[T] = token.asInstanceOf[DFToken[T]]

object DFToken:
  extension [T <: ir.DFType, A <: Args](token: DFToken[DFType[T, A]])
    def asIR: ir.DFToken[T] = (token.value: @unchecked) match
      case tokenIR: ir.DFToken[T] => tokenIR
      case err: DFError           => throw DFError.Derived(err)
    def codeString(using printer: Printer)(using DFC): String =
      printer.csDFToken(asIR)
  extension [T <: ir.DFType, Data](
      token: DFToken[DFType[ir.DFType.Aux[T, Data], Args]]
  ) def data: Data = token.asIR.data.asInstanceOf[Data]

  protected[core] def bubble[T <: DFTypeAny](dfType: T): DFToken[T] =
    ir.DFToken.bubble(dfType.asIR).asTokenOf[T]

  @implicitNotFound("Unsupported token value ${V} for DFHDL type ${T}")
  trait TC[T <: DFTypeAny, V] extends TCConv[T, V, DFTokenAny]:
    type Out = DFToken[T]
    type Ctx = DummyImplicit
    final def apply(dfType: T, value: V): Out = conv(dfType, value)

  object TC

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

  object Compare

end DFToken
