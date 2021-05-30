package DFiant
package core
import compiler.printing.*
import internals.*
import scala.annotation.targetName
import compiletime.*
import scala.quoted.*
import collection.mutable

sealed trait DFType extends NCCode, Product, Serializable:
  type Width <: Int
  protected val width: Int

object DFType:
  trait TC[T]:
    type Type <: DFType
    def apply(t: T): Type

  transparent inline given ofDFType[T <: DFType]: TC[T] =
    new TC[T]:
      type Type = T
      def apply(t: T): Type = t

  extension [T](t: T)(using tc: TC[T])
    def dfType: tc.Type = tc(t)
    def width(using w: Width[tc.Type]): Inlined.Int[w.Out] =
      Inlined.Int.forced[w.Out](dfType.width)
    def codeString(using Printer): String = dfType.codeString
    def <>(dir: Int): Unit = {}

  type Supported = DFType | DFStruct.Fields | Tuple
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
        case applied: AppliedType if applied <:< TypeRepr.of[Tuple] =>
          val widths = applied.args.map(a => a.calcWidth)
          if (widths.forall(_.nonEmpty)) Some(widths.flatten.sum)
          else None
        case applied: AppliedType if applied <:< TypeRepr.of[DFTuple[_]] =>
          applied.args.head.calcWidth
        case fieldsTpe if fieldsTpe <:< TypeRepr.of[DFStruct.Fields] =>
          val fieldTpe = TypeRepr.of[DFStruct.Field[_]]
          val clsSym = fieldsTpe.classSymbol.get
          val widths =
            clsSym.declaredFields.view
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

  type Of[T] <: DFType = T match
    case DFBit.type  => DFBit.type
    case DFBool.type => DFBool.type
    case DFBits[w]   => DFBits[w]
    case DFTuple[t]  => DFTuple[t]
    case Tuple       => DFTuple[Tuple.Map[T, Of]]
  // inline def doit[T <: Tuple](inline t: T): List[DFType] =
  //   t.map[Of]([T] => (t: T) => of(t)).toList.asInstanceOf[List[DFType]]

  // inline def of[T](inline t: T): Of[T] =
  //   inline t match
  //     case dfType: DFType => dfType.asInstanceOf[Of[T]]
  //     case tuple: Tuple   => DFTuple[Tuple](doit(tuple)).asInstanceOf[Of[T]]

  // inline def fromTuple[T](inline tpl : T, list : List[DFType]) : DFTuple = tpl match
  //   case EmptyTuple => DFTuple[T]()
  // inline def apply[T](inline t : T) : DFType =
  //   inline t match
  //     case dfType : DFType => dfType
  //     case tuple : Tuple => ???

  /////////////////////////////////////////////////////////////////////////////
  // DFBool or DFBit
  /////////////////////////////////////////////////////////////////////////////
  sealed trait DFBoolOrBit extends DFType:
    type Width = 1
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
    type Width = W
    def codeString(using Printer): String = s"DFBits($width)"
  object DFBits:
    def apply[W <: Int](width: Inlined.Int[W]): DFBits[W] = DFBits[W](width)
    @targetName("applyNoArg")
    def apply[W <: Int with Singleton](using ValueOf[W]): DFBits[W] =
      DFBits[W](valueOf[W])
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
      extension [T](t: T)(using tc: TC[T])
        def <>(FIELD: FIELD)(using MetaContext): DFStruct.Field[tc.Type] =
          val dfType = tc(t)
          val field = DFStruct.Field(dfType)
          all += field
          field
    object Fields

    final case class Field[Type <: DFType](dfType: Type)(using MetaContext)

  /////////////////////////////////////////////////////////////////////////////
  // DFTuple
  /////////////////////////////////////////////////////////////////////////////
  case class DFTuple[T <: Tuple] private (dfTypeList: List[DFType])
      extends DFType:
    protected[DFType] val width: Int = dfTypeList.view.map(_.width).sum
    def codeString(using Printer): String =
      dfTypeList.view.map(_.codeString).mkString("(", ", ", ")")

  object DFTuple:
    def apply[T <: Tuple](tuple: T)(using of: Of[T]): DFTuple[T] =
      new DFTuple[T](of(tuple))
    trait Of[T <: Tuple]:
      def apply(t: T): List[DFType]
    object Of:
      import ops.int.+
      import DFType.TC
      // inline given [T <: Tuple]: Of[T] =
      //   new Of[T]:
      //     def apply(t: T): List[DFType] =
      //       t.map[[X] =>> DFType]([T] => (t: T) => summonInline[TC[T]](t))
      //         .toList
      //         .asInstanceOf[List[DFType]]
      inline given [T1, T2](using
          tc1: TC[T1],
          tc2: TC[T2]
      ): Of[(T1, T2)] =
        new Of[(T1, T2)]:
          def apply(t: (T1, T2)): List[DFType] = List(tc1(t._1), tc2(t._2))

  transparent inline given ofTuple[T <: Tuple](using
      of: DFTuple.Of[T],
      w: Width[T]
  ): TC[T] =
    new TC[T]:
      type Type = DFTuple[T]
      def apply(t: T): Type = DFTuple(t)

/////////////////////////////////////////////////////////////////////////////
