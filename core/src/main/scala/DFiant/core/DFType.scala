package DFiant
package core
import compiler.printing.*
import internals.*
import scala.annotation.targetName
import compiletime.*
import scala.quoted.*
import collection.mutable
import collection.immutable.ListMap

sealed trait DFType extends NCCode: //, Product, Serializable
  type TokenData
  protected[DFiant] def tokenDataToBits(
      data: TokenData
  ): (BitVector, BitVector) = ???
  protected[DFiant] def tokenBitsToData(
      valueBits: BitVector,
      bubbleBits: BitVector
  ): TokenData = ???
  protected[DFiant] def tokenCodeString(data: TokenData)(using
      Printer
  ): String = ???
  protected[DFiant] def tokenEquals(
      lhs: DFToken[?],
      rhs: DFToken[?]
  ): DFBool.Token = ???
  protected[DFiant] def tokenBubble: DFToken[?] = ???
  val __width: Int

object DFType:
  private[core] def apply(t: Any): DFType =
    t match
      case dfType: DFType       => dfType
      case tuple: NonEmptyTuple => DFTuple(tuple)
      case fields: DFFields     => DFStruct(fields)
      //TODO: need to add proper upper-bound if fixed in Scalac
      //see: https://contributors.scala-lang.org/t/missing-dedicated-class-for-enum-companions
      case enumCompanion: AnyRef => DFEnum(enumCompanion)

  sealed trait DFMatchable extends DFType
  sealed trait DFFlattenable extends DFType
  def tcMacro[T <: AnyRef](using Quotes, Type[T]): Expr[TC[T]] =
    import quotes.reflect.*
    val tTpe = TypeRepr.of[T]
    val dfTypeTpe = TypeRepr.of[DFType]
    val nonEmptyTupleTpe = TypeRepr.of[NonEmptyTuple]
    val fieldTpe = TypeRepr.of[DFField[_]]
    val fieldsTpe = TypeRepr.of[DFFields]
    val encodingSym = TypeRepr.of[DFEncoding].typeSymbol
    def checkSupported(tTpe: TypeRepr): Unit = {
      // println((tTpe.show, expr.show))
      tTpe match
        case t if t <:< dfTypeTpe =>
        case applied: AppliedType if applied <:< nonEmptyTupleTpe =>
          applied.args.foreach(checkSupported)
        case t if t <:< fieldsTpe =>
        case DFEnum(_)            =>
        case t =>
          report.error(s"Unsupported dataflow type can be found for: ${t.show}")
    }

    checkSupported(tTpe)
    tTpe match
      case t if t <:< nonEmptyTupleTpe =>
        '{
          new TC[T]:
            type Type = DFTuple[T]
            def apply(t: T): Type = DFTuple[T](t)
        }
      case DFEnum(entries) =>
        val clsType = entries.head.asType
        clsType match
          case '[e] =>
            '{
              new TC[T]:
                type Type = DFEnum[T, e]
                def apply(t: T): Type = DFEnum[T, e](t)
            }

  trait TC[T]:
    type Type <: DFType
    def apply(t: T): Type

  given ofDFType[T <: DFType]: TC[T] with
    type Type = T
    def apply(t: T): Type = t
  given ofDFFields[T <: DFFields]: TC[T] with
    type Type = DFStruct[T]
    def apply(t: T): Type = DFStruct[T](t)
  transparent inline given ofAnyRef[T <: AnyRef]: TC[T] = ${ tcMacro[T] }

  type Supported = AnyRef //DFType | NonEmptyTuple
  object Ops:
    extension [T <: Supported](t: T)(using tc: TC[T])
      def dfType: tc.Type = tc(t)
      def width(using w: Width[T]): Inlined.Int[w.Out] =
        Inlined.Int.forced[w.Out](dfType.__width)
      def codeString(using Printer): String = dfType.codeString
      // transparent inline def X(inline cellDim: Int*): DFType =
      //   x(dfType, cellDim: _*)
      inline def X(
          inline cellDim: Int
      ): DFVector[tc.Type, Tuple1[cellDim.type]] =
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
      ): DFVector[tc.Type, Tuple3[
        cellDim0.type,
        cellDim1.type,
        cellDim2.type
      ]] =
        DFVector(dfType, Tuple3(cellDim0, cellDim1, cellDim2))
      transparent inline def opaque(using MetaContext): DFOpaque[_] =
        DFOpaque(dfType)
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


/////////////////////////////////////////////////////////////////////////////
// DFBool or DFBit
/////////////////////////////////////////////////////////////////////////////
sealed trait DFBoolOrBit extends DFType.DFMatchable:
  type TokenData = (Boolean, Boolean)
  final val __width = 1
object DFBoolOrBit extends DFBoolOrBitCompanion:
  type Token = DFToken[DFBoolOrBit]

case object DFBool extends DFBoolOrBit:
  type Token = DFToken[DFBool.type]
  def codeString(using Printer): String = "DFBool"
type DFBool = DFBool.type
case object DFBit extends DFBoolOrBit:
  type Token = DFToken[DFBit.type]
  def codeString(using Printer): String = "DFBit"
type DFBit = DFBit.type
/////////////////////////////////////////////////////////////////////////////

/////////////////////////////////////////////////////////////////////////////
// DFBits
/////////////////////////////////////////////////////////////////////////////
final case class DFBits[W <: Int] private (
    val __width: Int
) extends DFType.DFMatchable:
  type TokenData = (BitVector, BitVector)
  override def tokenDataToBits(data: TokenData): (BitVector, BitVector) = data
  override def tokenBubble: DFBits.Token[Int] =
    DFBits.Token.bubble(Inlined.Int(__width))
  def codeString(using Printer): String = s"DFBits(${__width})"

object DFBits extends DFBitsCompanion:
  def apply[W <: Int](width: Inlined.Int[W]): DFBits[W] = new DFBits[W](width)
  @targetName("applyNoArg")
  def apply[W <: Int with Singleton](using ValueOf[W]): DFBits[W] =
    new DFBits[W](valueOf[W])
/////////////////////////////////////////////////////////////////////////////

/////////////////////////////////////////////////////////////////////////////
// DFEnum
/////////////////////////////////////////////////////////////////////////////
final case class DFEnum[C <: AnyRef, E](
    val name: String,
    val __width: Int,
    val entries: ListMap[String, BigInt]
) extends DFType.DFMatchable:
  def codeString(using Printer): String = name
object DFEnum:
  def unapply(using Quotes)(
      tpe: quotes.reflect.TypeRepr
  ): Option[List[quotes.reflect.TypeRepr]] =
    import quotes.reflect.*
    val enumTpe = TypeRepr.of[scala.reflect.Enum]
    val sym = tpe.termSymbol
    if (sym.companionClass.flags.is(Flags.Enum))
      Some(
        sym.declaredFields.view
          .map(f => tpe.memberType(f))
          .filter(_ <:< enumTpe)
          .toList
      )
    else None
  def apply[C <: AnyRef, E](enumCompanion: C): DFEnum[C, E] =
    val enumClass = classOf[scala.reflect.Enum]
    val enumCompanionCls = enumCompanion.getClass
    val fieldsAsPairs = for (
      field <- enumCompanionCls.getDeclaredFields
      if enumClass.isAssignableFrom(field.getType)
    ) yield {
      field.setAccessible(true)
      (field.getName, field.get(enumCompanion).asInstanceOf[DFEncoding])
    }
    val name = enumCompanionCls.getSimpleName.replace("$", "")
    val width = fieldsAsPairs.head._2.calcWidth(fieldsAsPairs.size)
    val entryPairs = fieldsAsPairs.zipWithIndex.map {
      case ((name, entry), idx) => (name, entry.value)
    }
    DFEnum[C, E](name, width, ListMap(entryPairs: _*))
/////////////////////////////////////////////////////////////////////////////

/////////////////////////////////////////////////////////////////////////////
// DFVector
/////////////////////////////////////////////////////////////////////////////
final case class DFVector[T <: DFType, D <: NonEmptyTuple](
    cellType: T,
    cellDim: D
) extends DFType.DFFlattenable:
  val __width: Int =
    cellType.__width * cellDim.toList.asInstanceOf[List[Int]].reduce(_ * _)
  def codeString(using Printer): String =
    s"${cellType.codeString}.X${cellDim.toList.mkString("(", ", ", ")")}"
/////////////////////////////////////////////////////////////////////////////

/////////////////////////////////////////////////////////////////////////////
// DFOpaque
/////////////////////////////////////////////////////////////////////////////
abstract class DFOpaque[T <: DFType](
    actualType: T
)(using meta: MetaContext)
    extends DFType.DFFlattenable:
  final val __width: Int = actualType.__width
  final def codeString(using Printer): String = meta.name
object DFOpaque:
  transparent inline def apply[T <: DFType](actualType: T)(using
      MetaContext
  ): DFOpaque[_] =
    new DFOpaque[T](actualType) {}
/////////////////////////////////////////////////////////////////////////////

/////////////////////////////////////////////////////////////////////////////
// DFUnion
/////////////////////////////////////////////////////////////////////////////
final case class DFUnion[F <: DFFields](fieldsSet: Set[DFType]) extends DFType:
  val __width: Int = fieldsSet.head.__width
  def codeString(using Printer): String =
    fieldsSet.map(_.codeString).mkString(" | ")
object DFUnion:
  trait Able[T]:
    type F <: DFFields
    def apply(t: T): DFUnion[F]
  object Able:
    transparent inline given fromFields[F0 <: DFFields]: Able[F0] =
      new Able[F0]:
        type F = F0
        def apply(t: F0): DFUnion[F] = DFUnion[F](???)
    transparent inline given fromUnion[F0 <: DFFields]: Able[DFUnion[F0]] =
      new Able[DFUnion[F0]]:
        type F = F0
        def apply(t: DFUnion[F0]): DFUnion[F] = t
  object Ops:
    extension [L](lhs: L)(using l: Able[L])
      def |[R](rhs: R)(using r: Able[R]): DFUnion[l.F | r.F] = ???
/////////////////////////////////////////////////////////////////////////////

/////////////////////////////////////////////////////////////////////////////
// DFStruct
/////////////////////////////////////////////////////////////////////////////
final case class DFStruct[F <: DFFields](
    name: String,
    fieldMap: ListMap[String, DFType]
) extends DFType.DFFlattenable:
  val __width: Int = fieldMap.values.map(_.__width).sum
  def codeString(using Printer): String = name
object DFStruct:
  def apply[F <: DFFields](fields: F): DFStruct[F] =
    val fieldMap = ListMap(fields.getFields.map(f => (f.name, f.dfType)): _*)
    DFStruct[F](fields.name, fieldMap)
/////////////////////////////////////////////////////////////////////////////

/////////////////////////////////////////////////////////////////////////////
// DFTuple
/////////////////////////////////////////////////////////////////////////////
final case class DFTuple[T <: AnyRef](dfTypeList: List[DFType])
    extends DFType.DFMatchable,
      DFType.DFFlattenable:
  val __width: Int = dfTypeList.view.map(_.__width).sum
  def codeString(using Printer): String =
    dfTypeList.view.map(_.codeString).mkString("(", ", ", ")")

object DFTuple:
  def apply[T <: AnyRef](t: T): DFTuple[T] =
    val dfTypeList: List[DFType] =
      t.asInstanceOf[NonEmptyTuple]
        .toList
        //TODO: Hack due to https://github.com/lampepfl/dotty/issues/12721
        .asInstanceOf[List[AnyRef]]
        .map(DFType.apply)
    DFTuple[T](dfTypeList)

/////////////////////////////////////////////////////////////////////////////