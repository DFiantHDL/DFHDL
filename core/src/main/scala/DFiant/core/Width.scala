package DFiant
package core
import internals.*
import scala.quoted.*
import compiler.ir

trait Width[T]:
  type Out <: Int
object Width:
  val wide: Width[DFTypeAny] = new Width[DFTypeAny]:
    type Out = Int
  given fromDFBoolOrBit[T <: DFBoolOrBit]: Width[T] with
    type Out = 1
  given fromDFBits[W <: Int]: Width[DFBits[W]] with
    type Out = W
  given fromDFDecimal[S <: Boolean, W <: Int, F <: Int]
      : Width[DFDecimal[S, W, F]] with
    type Out = W
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
        case t if t <:< TypeRepr.of[DFTypeAny] =>
          val AppliedType(_, List(irTpe, tuple)) = t.dealias
          val args: List[TypeRepr] = tuple match
            case AppliedType(_, args) => args
            case _                    => Nil
          irTpe match
            case t if t <:< TypeRepr.of[ir.DFBoolOrBit] =>
              ConstantType(IntConstant(1))
            case t if t <:< TypeRepr.of[ir.DFBits] =>
              args.head.simplify
            case t if t <:< TypeRepr.of[ir.DFDecimal] =>
              args(1).simplify
            case t if t <:< TypeRepr.of[ir.DFEnum] =>
              args.head.calcWidth
            case t if t <:< TypeRepr.of[ir.DFVector] =>
              val cellWidth = args.head.calcWidth
              val cellDims = args.last.asInstanceOf[AppliedType].args
              val widths = cellWidth :: cellDims
              widths.reduce(_ * _)
            case t if t <:< TypeRepr.of[ir.DFOpaque] =>
              args.head.calcWidth
            case t if t <:< TypeRepr.of[ir.DFStruct] =>
              args.head.calcWidth
            case t if t <:< TypeRepr.of[ir.DFUnion] =>
              args.head.calcWidth
            case _ =>
              TypeRepr.of[Int]
          end match
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
        case opaqueFE if opaqueFE <:< TypeRepr.of[DFOpaque.Abstract] =>
          val clsSym = opaqueFE.classSymbol.get
          opaqueFE.memberType(clsSym.fieldMember("actualType")).calcWidth
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
      end match
    end calcWidth
  end extension
  def getWidthMacro[T](using Quotes, Type[T]): Expr[Width[T]] =
    import quotes.reflect.*
    val tTpe = TypeRepr.of[T]
//    println(tTpe.show)
    val widthTpe = tTpe.calcWidth.asTypeOf[Int]
    '{
      new Width[T]:
        type Out = widthTpe.Underlying
    }
end Width

extension [T <: DFTypeAny, M <: ir.DFVal.Modifier](dfVal: DFVal[T, M])
  def width(using w: Width[T]): Inlined[w.Out] =
    Inlined.forced[w.Out](dfVal.asIR.dfType.width)

extension [T <: DFTypeAny](token: DFToken[T])
  def width(using w: Width[T]): Inlined[w.Out] =
    Inlined.forced[w.Out](token.asIR.width)

extension [T](t: T)(using tc: DFType.TC[T])
  def width(using w: Width[T]): Inlined[w.Out] =
    Inlined.forced[w.Out](tc(t).asIR.width)
