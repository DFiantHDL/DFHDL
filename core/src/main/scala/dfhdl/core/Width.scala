package dfhdl.core
import dfhdl.internals.*
import dfhdl.compiler.ir
import scala.quoted.*
import annotation.targetName
import scala.annotation.nowarn

trait Width[T]:
  type Out <: IntP
  type OutI <: Int
trait WidthLP:
  given fromDFBitsIntP[W <: IntP]: Width[DFBits[W]] with
    type Out = W
    type OutI = Int
  given fromDFDecimalIntP[S <: Boolean, W <: IntP, F <: Int, N <: ir.DFDecimal.NativeType]
      : Width[DFDecimal[S, W, F, N]] with
    type Out = W
    type OutI = Int
object Width extends WidthLP:
  type Aux[T, O <: IntP] = Width[T] { type Out = O }
  type AuxI[T, O <: Int] = Width[T] { type OutI = O }
  val wide: Width[DFTypeAny] = new Width[DFTypeAny]:
    type Out = Int
    type OutI = Int
  given fromDFBoolOrBit[T <: DFBoolOrBit]: Width[T] with
    type Out = 1
    type OutI = 1
  given fromBooleanCompanion: Width[Boolean.type] with
    type Out = 1
    type OutI = 1
  given fromDFDouble: Width[DFDouble] with
    type Out = 64
    type OutI = 64
  given fromDoubleCompanion: Width[Double.type] with
    type Out = 64
    type OutI = 64
  given fromDFBitsInt[W <: Int]: Width[DFBits[W]] with
    type Out = W
    type OutI = W
  given fromDFDecimalInt[S <: Boolean, W <: Int, F <: Int, N <: ir.DFDecimal.NativeType]
      : Width[DFDecimal[S, W, F, N]] with
    type Out = W
    type OutI = W
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
    infix def max(rhs: quotes.reflect.TypeRepr): quotes.reflect.TypeRepr =
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
            case '[DFDouble] =>
              ConstantType(IntConstant(64))
            case '[DFBits[w]] =>
              Type.of[w] match
                case '[DFValAny] => TypeRepr.of[Int]
                case _           =>
                  TypeRepr.of[w].calcWidth
            case '[DFDecimal[s, w, f, n]] =>
              Type.of[w] match
                case '[DFValAny] => TypeRepr.of[Int]
                case _           =>
                  TypeRepr.of[w].calcWidth
            case '[DFEnum[e]] =>
              TypeRepr.of[e].calcWidth
            case '[DFVector[t, d]] =>
              val cellWidth = TypeRepr.of[t].calcWidth
              val cellDims = TypeRepr.of[d].getTupleArgs.map { t =>
                t.asTypeOf[Any] match
                  case '[DFValAny] => TypeRepr.of[Int]
                  case _           => t.calcWidth
              }
              val widths = cellWidth :: cellDims
              widths.reduce(_ * _)
            case '[DFOpaque[t]] =>
              TypeRepr.of[t].calcWidth
            case '[DFTuple[t]] =>
              TypeRepr.of[t].getTupleArgs.map(_.calcValWidth).reduce(_ + _)
            case '[DFStruct[p]] =>
              TypeRepr.of[p].calcWidth
            // TODO: figure out why this is needed and DFVector case is not taken
            case '[DFType[ir.DFVector, Args2[t, d]]] =>
              val cellWidth = TypeRepr.of[t].calcWidth
              val cellDims = TypeRepr.of[d].getTupleArgs.map { t =>
                t.asTypeOf[Any] match
                  case '[DFValAny] => TypeRepr.of[Int]
                  case _           => t.calcWidth
              }
              val widths = cellWidth :: cellDims
              widths.reduce(_ * _)
            case _ =>
              TypeRepr.of[Int]
          end match
        // TODO: maybe in the future we use IntP.Sig and further calculate the width according the typelevel operations
        case '[IntP.Sig]      => TypeRepr.of[Int]
        case '[Int]           => dfTpe
        case '[Boolean.type]  => ConstantType(IntConstant(1))
        case '[Double.type]   => ConstantType(IntConstant(64))
        case '[Byte.type]     => ConstantType(IntConstant(8))
        case '[Int.type]      => ConstantType(IntConstant(32))
        case '[Long.type]     => ConstantType(IntConstant(64))
        case '[NonEmptyTuple] =>
          val widths =
            dfTpe.getTupleArgs.map(a => a.calcWidth)
          widths.reduce(_ + _)
        case '[DFOpaque.Abstract] =>
          val clsSym = dfTpe.classSymbol.get
          dfTpe.memberType(clsSym.fieldMember("actualType")).calcWidth
        case '[DFStruct.Fields] =>
          dfTpe.asTypeOf[Any] match
            case '[NonEmptyTuple] =>
              // TODO: revisit why there is a warning.
              // See: https://github.com/lampepfl/dotty/issues/18507
              @nowarn("msg=Unreachable case")
              val ret = dfTpe.getTupleArgs
                .map(_.asTypeOf[AnyKind])
                .collect { case '[DFValOf[t]] =>
                  TypeRepr.of[t].calcWidth
                }
                .reduce(_ + _)
              ret
            case _ =>
              val clsSym = dfTpe.classSymbol.get
              // TODO: revisit why there is a warning.
              // See: https://github.com/lampepfl/dotty/issues/18507
              @nowarn("msg=Unreachable case")
              val widths =
                clsSym.fieldMembers.view
                  .map(m => dfTpe.memberType(m).asTypeOf[AnyKind])
                  .collect { case '[DFValOf[t]] =>
                    TypeRepr.of[t].calcWidth
                  }
              widths.reduce(_ + _)
          end match
        case _ =>
          dfTpe match
            case DFEnum(entries) =>
              val entryCount = entries.length
              val widthOption = entries.head.asType match
                case '[DFEncoding.Toggle] =>
                  if (entryCount != 2)
                    throw new IllegalArgumentException(
                      s"Toggle enumeration must have exactly 2 entries, but found $entryCount"
                    )
                  Some(1)
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
                      if ((entryCount).bitsWidth(false) > value)
                        report.errorAndAbort(
                          s"Explicit width $value is too small for $entryCount entries"
                        )
                      else
                        Some(value)
                    case _ => None
              widthOption
                .map(w => ConstantType(IntConstant(w)))
                .getOrElse(TypeRepr.of[Int])
            case OrType(left, right) =>
              left.calcWidth max right.calcWidth
            case compObjTpe =>
              val compPrefix = compObjTpe match
                case TermRef(pre, _) => pre
                case _               =>
                  report.errorAndAbort("Case class companion must be a term ref")
              val clsSym = compObjTpe.typeSymbol.companionClass
              if !clsSym.paramSymss.forall(_.headOption.forall(_.isTerm)) then
                report.errorAndAbort(
                  "Case class with type parameters are not supported"
                )
              compPrefix.select(clsSym).calcWidth

          end match
      end match
    end calcWidth
    def calcValWidth: quotes.reflect.TypeRepr =
      import quotes.reflect.*
      dfTpe.asType match
        case '[DFVal[t, m]] =>
          TypeRepr.of[t].calcWidth
        case '[NonEmptyTuple] =>
          val args = dfTpe.getTupleArgs
          val widths = args.map(a => a.calcValWidth)
          widths.reduce(_ + _)
        case _ =>
          dfTpe.dealias match
            case ConstantType(IntConstant(v)) if (v == 1 || v == 0) =>
              ConstantType(IntConstant(1))
            case ref: TermRef if ref.termSymbol.name != "<none>" =>
              ref.widen.calcValWidth
            case x =>
              report.errorAndAbort(
                s"Unsupported argument value ${x.showType} for DFHDL receiver type DFBits"
              )
      end match
    end calcValWidth
  end extension
  object Success extends Width[Any]
  def getWidthMacro[T](using Quotes, Type[T]): Expr[Width[T]] =
    import quotes.reflect.*
    val tTpe = TypeRepr.of[T]
//    println(tTpe.show)
    val widthTpe = tTpe.calcWidth.asTypeOf[Int]
    '{
      Success.asInstanceOf[
        Width[T] {
          type Out = widthTpe.Underlying
          type OutI = widthTpe.Underlying
        }
      ]
    }
end Width

extension [T <: DFTypeAny, M <: ModifierAny](dfVal: DFVal[T, M])
  @targetName("dfValWidth")
  def widthInt(using dfc: DFC, w: Width[T]): Inlined[w.OutI] =
    import dfc.getSet
    Inlined.forced[w.OutI](dfVal.asIR.dfType.width)
  def widthIntParam(using dfc: DFC, w: Width[T]): IntParam[w.Out] =
    import dfc.getSet
    dfVal.dfType.widthIntParam

extension [T](t: T)(using tc: DFType.TC[T])
  @targetName("tWidth")
  def widthInt(using dfc: DFC, w: Width[tc.Type]): Inlined[w.OutI] =
    import dfc.getSet
    Inlined.forced[w.OutI](tc(t).asIR.width)
  def widthIntParam(using dfc: DFC, w: Width[tc.Type]): IntParam[w.Out] =
    import dfc.getSet
    def intParam(dfTypeIR: ir.DFType): IntParam[Int] = dfTypeIR match
      case ir.DFBits(width)                        => width.get
      case ir.DFXInt(_, width, _)                  => width.get
      case ir.DFVector(cellType, cellDimParamRefs) =>
        intParam(cellType) * cellDimParamRefs.map(_.get).asInstanceOf[List[IntParam[Int]]].reduce(
          (l, r) => (l * r).asInstanceOf[IntParam[Int]]
        )
      case ir.DFStruct(_, fieldMap) =>
        fieldMap.values.map(intParam).reduce(_ + _).asInstanceOf[IntParam[Int]]
      case _ => IntParam.forced[Int](dfTypeIR.width)
    intParam(tc(t).asIR).asInstanceOf[IntParam[w.Out]]
end extension
