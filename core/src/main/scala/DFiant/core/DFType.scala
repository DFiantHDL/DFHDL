package DFiant
package core
import compiler.printing.*
import internals.*
import scala.annotation.targetName
import compiletime.*
import scala.quoted.*
import collection.mutable

sealed trait DFType extends NCCode, Product, Serializable:
  protected val width: Int

object DFType:
  protected def apply[T <: Supported](t: T): DFType =
    t match
      case dfType: DFType => dfType
      case tpl: NonEmptyTuple =>
        new DFTuple(tpl.toList.asInstanceOf[List[Supported]].map(apply))
      case fields: DFStruct.Fields =>
        new DFStruct(fields)

  trait TC[T]:
    type Type <: DFType
    def apply(t: T): Type

  transparent inline given ofDFType[T <: DFType]: TC[T] =
    new TC[T]:
      type Type = T
      def apply(t: T): Type = t

  type Supported = DFType | DFStruct.Fields | NonEmptyTuple

  extension [T <: Supported](t: T)(using tc: TC[T], w: Width[T])
    def dfType: tc.Type = tc(t)
    def width: Inlined.Int[w.Out] =
      Inlined.Int.forced[w.Out](dfType.width)
    def codeString(using Printer): String = dfType.codeString
    // transparent inline def X(inline cellDim: Int*): DFType =
    //   x(dfType, cellDim: _*)
    inline def X(inline cellDim: Int): DFVector[tc.Type, Tuple1[cellDim.type]] =
      DFVector(dfType, Tuple1(cellDim))
    inline def X(
        inline cellDim0: Int,
        inline cellDim1: Int
    ): DFVector[tc.Type, Tuple2[cellDim0.type, cellDim1.type]] =
      DFVector(dfType, Tuple2(cellDim0, cellDim1))
    inline def X(
        inline cellDim0: Int,
        inline cellDim1: Int,
        inline cellDim2: Int
    ): DFVector[tc.Type, Tuple3[cellDim0.type, cellDim1.type, cellDim2.type]] =
      DFVector(dfType, Tuple3(cellDim0, cellDim1, cellDim2))
    def <>(dir: Int): Unit = {}

  // transparent inline def x[T <: DFType](
  //     cellType: T,
  //     inline cellDim: Int*
  // ): DFType =
  //   ${ xMacro('cellType, 'cellDim) }
  // def xMacro[T <: DFType](cellType: Expr[T], cellDim: Expr[Seq[Int]])(using
  //     Quotes,
  //     Type[T]
  // ): Expr[DFType] =
  //   import quotes.reflect.*
  //   val (tpe, tpl) = cellDim match
  //     case Varargs(argExprs) =>
  //       argExprs match
  //         case arg :: Nil =>
  //           println(arg)
  //           val tp = ConstantType(IntConstant(5)).asType
  //           (
  //             TypeRepr.of[Tuple1[5]].asType.asInstanceOf[Type[Int]],
  //             '{ Tuple1($arg) }
  //           )
  //   '{ new DFVector[T, tpe.Underlying]($cellType, $tpl) }

  trait Width[T <: Supported]:
    type Out <: Int
  transparent inline given [T <: Supported]: Width[T] = ${ getWidthMacro[T] }
  extension (using quotes: Quotes)(dfTpe: quotes.reflect.TypeRepr)
    def calcWidth: Option[Int] =
      import quotes.reflect.*
      dfTpe match
        case t if dfTpe <:< TypeRepr.of[DFBoolOrBit] => Some(1)
        case applied: AppliedType if applied <:< TypeRepr.of[DFBits[_]] =>
          applied.args.head match
            case ConstantType(IntConstant(value)) => Some(value)
            case _                                => None
        case applied: AppliedType if applied <:< TypeRepr.of[DFVector[_, _]] =>
          val cellWidth = applied.args.head.calcWidth
          val cellDims = applied.args.last.asInstanceOf[AppliedType].args
          val widths = cellWidth :: cellDims.map {
            case ConstantType(IntConstant(value)) => Some(value)
            case _                                => None
          }
          if (widths.forall(_.nonEmpty)) Some(widths.flatten.reduce(_ * _))
          else None
        case applied: AppliedType if applied <:< TypeRepr.of[NonEmptyTuple] =>
          val widths = applied.args.map(a => a.calcWidth)
          if (widths.forall(_.nonEmpty)) Some(widths.flatten.sum)
          else None
        case applied: AppliedType if applied <:< TypeRepr.of[DFTuple[_]] =>
          applied.args.head.calcWidth
        case fieldsTpe if fieldsTpe <:< TypeRepr.of[DFStruct.Fields] =>
          val fieldTpe = TypeRepr.of[DFStruct.Field[_]]
          val clsSym = fieldsTpe.classSymbol.get
          val widths =
            clsSym.memberFields.view
              .map(fieldsTpe.memberType)
              .collect {
                case applied: AppliedType if applied <:< fieldTpe =>
                  applied.args.head.calcWidth
              }
          if (widths.forall(_.nonEmpty)) Some(widths.flatten.sum)
          else None
        case applied: AppliedType if applied <:< TypeRepr.of[DFStruct[_]] =>
          applied.args.head.calcWidth
  def getWidthMacro[T <: Supported](using Quotes, Type[T]): Expr[Width[T]] =
    import quotes.reflect.*
    val tTpe = TypeRepr.of[T]
    val widthTpe: Type[Int] =
      tTpe.calcWidth
        .map(w => ConstantType(IntConstant(w)))
        .getOrElse(TypeRepr.of[Int])
        .asType
        .asInstanceOf[Type[Int]]
    '{
      new Width[T] {
        type Out = widthTpe.Underlying
      }
    }

  /////////////////////////////////////////////////////////////////////////////
  // DFBool or DFBit
  /////////////////////////////////////////////////////////////////////////////
  sealed trait DFBoolOrBit extends DFType:
    final protected[DFType] val width = 1

  case object DFBool extends DFBoolOrBit:
    def codeString(using Printer): String = "DFBool"
  case object DFBit extends DFBoolOrBit:
    def codeString(using Printer): String = "DFBit"
  /////////////////////////////////////////////////////////////////////////////

  /////////////////////////////////////////////////////////////////////////////
  // DFBits
  /////////////////////////////////////////////////////////////////////////////
  final case class DFBits[W <: Int] private (
      protected[DFType] val width: Int
  ) extends DFType:
    def codeString(using Printer): String = s"DFBits($width)"
  object DFBits:
    def apply[W <: Int](width: Inlined.Int[W]): DFBits[W] = DFBits[W](width)
    @targetName("applyNoArg")
    def apply[W <: Int with Singleton](using ValueOf[W]): DFBits[W] =
      DFBits[W](valueOf[W])
  /////////////////////////////////////////////////////////////////////////////

  /////////////////////////////////////////////////////////////////////////////
  // DFVector
  /////////////////////////////////////////////////////////////////////////////
  final case class DFVector[T <: DFType, D <: NonEmptyTuple](
      cellType: T,
      cellDim: D
  ) extends DFType:
    protected[DFType] val width: Int =
      cellType.width * cellDim.toList.asInstanceOf[List[Int]].reduce(_ * _)
    def codeString(using Printer): String =
      s"${cellType.codeString}.X${cellDim.toList.mkString("(", ", ", ")")}"
  /////////////////////////////////////////////////////////////////////////////

  /////////////////////////////////////////////////////////////////////////////
  // DFStruct
  /////////////////////////////////////////////////////////////////////////////
  final case class DFStruct[F <: DFStruct.Fields](fields: F) extends DFType:
    def codeString(using Printer): String = fields.name
    protected[DFType] val width = fields.width

  object DFStruct:
    abstract class Fields(using meta: MetaContext):
      final private[DFStruct] val all =
        mutable.ListBuffer.empty[DFStruct.Field[_ <: DFType]]
      final private[DFType] lazy val width: Int = all.map(_.dfType.width).sum
      final val name: String = meta.clsNameOpt.get
      protected sealed trait FIELD
      protected object FIELD extends FIELD
      extension [T <: Supported](t: T)(using tc: TC[T])
        def <>(FIELD: FIELD)(using MetaContext): DFStruct.Field[tc.Type] =
          val dfType = tc(t)
          val field = DFStruct.Field(dfType)
          all += field
          field
    object Fields

    final case class Field[Type <: DFType](dfType: Type)(using MetaContext)

  transparent inline given ofStruct[T <: DFStruct.Fields]: TC[T] =
    new TC[T]:
      type Type = DFStruct[T]
      def apply(t: T): Type = DFStruct(t)
  /////////////////////////////////////////////////////////////////////////////

  /////////////////////////////////////////////////////////////////////////////
  // DFTuple
  /////////////////////////////////////////////////////////////////////////////
  final case class DFTuple[T <: NonEmptyTuple](
      dfTypeList: List[DFType]
  ) extends DFType:
    protected[DFType] val width: Int = dfTypeList.view.map(_.width).sum
    def codeString(using Printer): String =
      dfTypeList.view.map(_.codeString).mkString("(", ", ", ")")

  object DFTuple

  transparent inline given ofTuple[T <: NonEmptyTuple](using
      w: Width[T]
  ): TC[T] =
    new TC[T]:
      type Type = DFTuple[T]
      def apply(t: T): Type = DFType(t).asInstanceOf[Type]
/////////////////////////////////////////////////////////////////////////////
