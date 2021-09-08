package DFiant
package core
import internals.*
import scala.quoted.*
import compiler.ir

trait Width[T]:
  type Out <: Int
object Width:
  transparent inline given [T]: Width[T] = ${ getWidthMacro[T] }
  extension (using quotes: Quotes)(dfTpe: quotes.reflect.TypeRepr)
    def +(rhs: quotes.reflect.TypeRepr): quotes.reflect.TypeRepr =
      import quotes.reflect.*
      (dfTpe, rhs) match
        case (ConstantType(IntConstant(l)), ConstantType(IntConstant(r))) =>
          ConstantType(IntConstant(l + r))
        case (l, r) if l =:= TypeRepr.of[Int] || r =:= TypeRepr.of[Int] =>
          TypeRepr.of[Int]
        case _ =>
          TypeRepr.of[scala.compiletime.ops.int.+].appliedTo(List(dfTpe, rhs))
    def *(rhs: quotes.reflect.TypeRepr): quotes.reflect.TypeRepr =
      import quotes.reflect.*
      (dfTpe, rhs) match
        case (ConstantType(IntConstant(l)), ConstantType(IntConstant(r))) =>
          ConstantType(IntConstant(l * r))
        case (l, r) if l =:= TypeRepr.of[Int] || r =:= TypeRepr.of[Int] =>
          TypeRepr.of[Int]
        case _ =>
          TypeRepr.of[scala.compiletime.ops.int.`*`].appliedTo(List(dfTpe, rhs))
    def max(rhs: quotes.reflect.TypeRepr): quotes.reflect.TypeRepr =
      import quotes.reflect.*
      (dfTpe, rhs) match
        case (ConstantType(IntConstant(l)), ConstantType(IntConstant(r))) =>
          ConstantType(IntConstant(l max r))
        case (l, r) if l =:= TypeRepr.of[Int] || r =:= TypeRepr.of[Int] =>
          TypeRepr.of[Int]
        case _ =>
          TypeRepr.of[scala.compiletime.ops.int.Max].appliedTo(List(dfTpe, rhs))
    def simplify: quotes.reflect.TypeRepr =
      import quotes.reflect.*
      dfTpe match
        case _: TermRef => TypeRepr.of[Int]
        case _          => dfTpe
    def calcWidth: quotes.reflect.TypeRepr =
      import quotes.reflect.*
      dfTpe match
        case t if dfTpe <:< TypeRepr.of[DFBoolOrBit] =>
          ConstantType(IntConstant(1))
        case applied: AppliedType if applied.tycon <:< TypeRepr.of[DFBits] =>
          applied.args.head.simplify
        case applied: AppliedType if applied.tycon <:< TypeRepr.of[DFDecimal] =>
          applied.args(1).simplify
        case applied: AppliedType if applied.tycon <:< TypeRepr.of[DFVector] =>
          val cellWidth = applied.args.head.calcWidth
          val cellDims = applied.args.last.asInstanceOf[AppliedType].args
          val widths = cellWidth :: cellDims
          widths.reduce(_ * _)
        case applied: AppliedType if applied <:< TypeRepr.of[NonEmptyTuple] =>
          val widths = applied.args.map(a => a.calcWidth)
          widths.reduce(_ + _)
        case fieldsTpe: AppliedType
            if fieldsTpe <:< TypeRepr.of[DFTuple.Fields[?]] =>
          fieldsTpe.args.head.calcWidth
        case fieldsTpe if fieldsTpe <:< TypeRepr.of[DFFields] =>
          val fieldTpe = TypeRepr.of[DFField[_]]
          val clsSym = fieldsTpe.classSymbol.get
          val widths =
            clsSym.fieldMembers.view
              .map(fieldsTpe.memberType)
              .collect {
                case applied: AppliedType if applied <:< fieldTpe =>
                  applied.args.head.calcWidth
              }
          widths.reduce(_ + _)
        case DFEnum(entries) =>
          val entryCount = entries.length
          val widthOption = entries.head match
            case DFEncoding.StartAt(startTpe) =>
              startTpe match
                case ConstantType(IntConstant(value)) =>
                  Some((entryCount - 1 + value).bitsWidth(false))
                case _ => None
            case t if t <:< TypeRepr.of[DFEncoding.OneHot] =>
              Some(entryCount)
            case t if t <:< TypeRepr.of[DFEncoding.Grey] =>
              Some((entryCount - 1).bitsWidth(false))
            case DFEncoding.Manual(widthTpe) =>
              widthTpe match
                case ConstantType(IntConstant(value)) =>
                  Some(value)
                case _ => None
          widthOption
            .map(w => ConstantType(IntConstant(w)))
            .getOrElse(TypeRepr.of[Int])
        case OrType(left, right) =>
          left.calcWidth max right.calcWidth
        case applied: AppliedType if applied.tycon <:< TypeRepr.of[DFOpaque] =>
          applied.args.last.calcWidth
        case applied: AppliedType if applied.tycon <:< TypeRepr.of[DFTuple] =>
          applied.args.head.calcWidth
        case applied: AppliedType if applied.tycon <:< TypeRepr.of[DFEnum] =>
          applied.args.head.calcWidth
        case applied: AppliedType if applied.tycon <:< TypeRepr.of[DFStruct] =>
          applied.args.head.calcWidth
        case applied: AppliedType if applied.tycon <:< TypeRepr.of[DFUnion] =>
          applied.args.head.calcWidth
        //lost specific type information, but still has non-literal width
        case t if t <:< TypeRepr.of[DFType] => TypeRepr.of[Int]
      end match
    end calcWidth
  end extension
  def getWidthMacro[T](using Quotes, Type[T]): Expr[Width[T]] =
    import quotes.reflect.*
    val tTpe = TypeRepr.of[T]
//    println(tTpe)
    val widthTpe: Type[Int] =
      tTpe.calcWidth.asType
        .asInstanceOf[Type[Int]]
    '{
      new Width[T]:
        type Out = widthTpe.Underlying
    }
end Width
