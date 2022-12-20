package dfhdl.core
import dfhdl.internals.*
import scala.quoted.*
import dfhdl.compiler.ir
import Modifier.*

extension [T](using quotes: Quotes)(tpe: quotes.reflect.TypeRepr)
  def showTuple(showf: quotes.reflect.TypeRepr => String): List[String] =
    import quotes.reflect.*
    tpe.asTypeOf[Tuple] match
      case '[field *: fields] =>
        showf(TypeRepr.of[field]) :: TypeRepr.of[fields].showTuple(showf)
      case '[EmptyTuple] =>
        Nil

  def showDFType: String =
    import quotes.reflect.*
    tpe.asTypeOf[DFTypeAny] match
      case '[DFBit]     => "Bit"
      case '[DFBool]    => "Boolean"
      case '[DFBits[w]] => s"Bits[${Type.show[w]}]"
      case '[DFUInt[w]] => s"UInt[${Type.show[w]}]"
      case '[DFSInt[w]] => s"SInt[${Type.show[w]}]"
      case '[DFEnum[t]] => Type.show[t]
      case '[DFVector[t, d]] =>
        s"${TypeRepr.of[t].showDFType} X ${TypeRepr.of[d].showType}"
      case '[DFType[ir.DFVector, Args2[t, d]]] =>
        s"${TypeRepr.of[t].showDFType} X ${TypeRepr.of[d].showType}"
      case '[DFOpaque[t]] => Type.show[t]
      case '[DFStruct[t]] =>
        Type.of[t] match
          case '[NonEmptyTuple] =>
            TypeRepr.of[t].showTuple(_.showType).mkStringBrackets
          case _ =>
            Type.show[t]
      case _ => "DFType"
    end match
  end showDFType

  def showModifier: String =
    import quotes.reflect.*
    tpe.asTypeOf[ModifierAny] match
      case '[Modifier.VAR] => "VAR"
      case _               => "VAL"

  def showDFVal: String =
    import quotes.reflect.*
    tpe.asTypeOf[DFValAny] match
      case '[DFVal[t, m]] =>
        s"${TypeRepr.of[t].showDFType} <> ${TypeRepr.of[m].showModifier}"

  def showDFToken: String =
    import quotes.reflect.*
    tpe.asTypeOf[DFTokenAny] match
      case '[DFToken[t]] =>
        s"${TypeRepr.of[t].showDFType} <> TOKEN"

  def showType: String =
    import quotes.reflect.*
    tpe.asTypeOf[Any] match
      case '[DFValAny]   => tpe.showDFVal
      case '[DFTokenAny] => tpe.showDFToken
      case '[DFTypeAny]  => tpe.showDFType
      case '[Tuple] =>
        tpe.showTuple(_.showType).mkStringBrackets
      case _ => tpe.show
end extension

trait ShowType[T]:
  type Out <: String
object ShowType:
  transparent inline given [T]: ShowType[T] = ${ macroImpl[T] }
  def macroImpl[T](using Quotes, Type[T]): Expr[ShowType[T]] =
    import quotes.reflect.*
    val shown =
      ConstantType(StringConstant(TypeRepr.of[T].showType)).asTypeOf[String]
    '{
      new ShowType[T]:
        type Out = shown.Underlying
    }
  end macroImpl
end ShowType
