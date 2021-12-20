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
              val cellDims = TypeRepr.of[d].getTupleArgs
              val widths = cellWidth :: cellDims
              widths.reduce(_ * _)
            case '[DFOpaque[t]] =>
              TypeRepr.of[t].calcWidth
            case '[DFStruct[p]] =>
              val pTpe = TypeRepr.of[p]
              pTpe.asTypeOf[Any] match
                case '[NonEmptyTuple] =>
                  pTpe.getTupleArgs
                    .map(_.asTypeOf[Any])
                    .collect { case '[DFValOf[t]] =>
                      TypeRepr.of[t].calcWidth
                    }
                    .reduce(_ + _)
                case _ =>
                  val clsSym = pTpe.classSymbol.get
                  val widths =
                    clsSym.fieldMembers.view
                      .map(m => pTpe.memberType(m).asTypeOf[Any])
                      .collect { case '[DFValOf[t]] =>
                        TypeRepr.of[t].calcWidth
                      }
                  widths.reduce(_ + _)
              end match
            // TODO: figure out why this is needed and DFVector case is not taken
            case '[DFType[ir.DFVector, Args2[t, d]]] =>
              val cellWidth = TypeRepr.of[t].calcWidth
              val cellDims = TypeRepr.of[d].getTupleArgs
              val widths = cellWidth :: cellDims
              widths.reduce(_ * _)
            case _ =>
              TypeRepr.of[Int]
          end match
        case '[Int] =>
          dfTpe
        case '[NonEmptyTuple] =>
          val widths =
            dfTpe.getTupleArgs.map(a => a.calcWidth)
          widths.reduce(_ + _)
        case '[DFOpaque.Abstract] =>
          val clsSym = dfTpe.classSymbol.get
          dfTpe.memberType(clsSym.fieldMember("actualType")).calcWidth
        case _ =>
          dfTpe match
            case DFEnum(entries) =>
              val entryCount = entries.length
              val widthOption = entries.head.asType match
                case '[DFEncoding.StartAt[t]] =>
                  TypeRepr.of[t] match
                    case ConstantType(IntConstant(value)) =>
                      Some((entryCount - 1 + value).bitsWidth(false))
                    case _ => None
                case '[DFEncoding.OneHot] =>
                  Some(entryCount)
                case '[DFEncoding.Grey] =>
                  Some((entryCount - 1).bitsWidth(false))
                case '[DFEncoding.Manual[w]] =>
                  TypeRepr.of[w] match
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
    def calcValWidth(onlyTokens: Boolean): quotes.reflect.TypeRepr =
      import quotes.reflect.*
      dfTpe.asType match
        case '[ValueOf[t]] =>
          TypeRepr.of[t].calcValWidth(onlyTokens)
        case '[DFVal[t, m]] if !onlyTokens =>
          TypeRepr.of[t].calcWidth
        case '[DFToken[t]] =>
          TypeRepr.of[t].calcWidth
        case '[NonEmptyTuple] =>
          val args = dfTpe.getTupleArgs
          val widths = args.map(a => a.calcValWidth(onlyTokens))
          widths.reduce(_ + _)
        case _ =>
          dfTpe.dealias match
            case ConstantType(IntConstant(v)) if (v == 1 || v == 0) =>
              ConstantType(IntConstant(1))
            case ref: TermRef =>
              ref.widen.calcValWidth(onlyTokens)
            case x =>
              report.errorAndAbort(
                s"Unsupported argument value ${x.showType} for dataflow receiver type DFBits"
              )
      end match
    end calcValWidth
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

extension [T <: DFType.Supported](t: T)
  def width(using w: Width[T]): Inlined[w.Out] =
    Inlined.forced[w.Out](DFType.of(t).asIR.width)
