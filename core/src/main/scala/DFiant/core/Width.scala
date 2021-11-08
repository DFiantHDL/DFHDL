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
      dfTpe.asTypeOf[Any] match
        case '[DFTypeAny] =>
          dfTpe.asTypeOf[DFTypeAny] match
            case '[DFBoolOrBit] =>
              ConstantType(IntConstant(1))
            case '[DFBits[w]] =>
              TypeRepr.of[w].calcWidth
            case '[DFDecimal[s, w, f]] =>
              TypeRepr.of[w].calcWidth
            case '[DFEnum[e]] =>
              TypeRepr.of[e].calcWidth
            case '[DFVector[t, d]] =>
              val cellWidth = TypeRepr.of[t].calcWidth
              val cellDims = TypeRepr.of[d].asInstanceOf[AppliedType].args
              val widths = cellWidth :: cellDims
              widths.reduce(_ * _)
            case '[DFOpaque[t]] =>
              TypeRepr.of[t].calcWidth
            case '[DFStruct[t]] =>
              TypeRepr.of[t].calcWidth
            case '[DFUnion[t]] =>
              TypeRepr.of[t].calcWidth
            case _ =>
              val AppliedType(_, List(irTpe, tuple)) = dfTpe.dealias
              val args: List[TypeRepr] = tuple match
                case AppliedType(_, args) => args
                case _                    => Nil
              irTpe match
                case t if t <:< TypeRepr.of[ir.DFVector] =>
                  val cellWidth = args.head.calcWidth
                  val cellDims = args.last.asInstanceOf[AppliedType].args
                  val widths = cellWidth :: cellDims
                  widths.reduce(_ * _)
                case _ =>
                  TypeRepr.of[Int]
          end match
        case '[Int] =>
          dfTpe
        case '[NonEmptyTuple] =>
          val widths =
            dfTpe.asInstanceOf[AppliedType].args.map(a => a.calcWidth)
          widths.reduce(_ + _)
        case '[DFTuple.Fields[t]] =>
          TypeRepr.of[t].calcWidth
        case '[DFFields] =>
          val clsSym = dfTpe.classSymbol.get
          val widths =
            clsSym.fieldMembers.view
              .map(dfTpe.memberType)
              .map(_.asTypeOf[Any])
              .collect { case '[DFField[t]] => TypeRepr.of[t].calcWidth }
          widths.reduce(_ + _)
        case '[DFOpaque.Abstract] =>
          val clsSym = dfTpe.classSymbol.get
          dfTpe.memberType(clsSym.fieldMember("actualType")).calcWidth
        case _ =>
          dfTpe match
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
