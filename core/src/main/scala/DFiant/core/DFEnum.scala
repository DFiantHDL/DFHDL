package DFiant
package core
import DFiant.compiler.ir
import scala.quoted.*
import internals.*
import collection.immutable.ListMap

sealed trait DFEncoding extends scala.reflect.Enum:
  def calcWidth(entryCount: Int): Int
  def encode(idx: Int): BigInt
  val value: BigInt

object DFEncoding:
  sealed trait Auto extends DFEncoding:
    val value: BigInt = encode(ordinal)
  abstract class Default extends StartAt(0)

  abstract class Grey extends Auto:
    final def calcWidth(entryCount: Int): Int =
      (entryCount - 1).bitsWidth(false)
    final def encode(idx: Int): BigInt = BigInt(idx ^ (idx >>> 1))

  abstract class StartAt[V <: Int with Singleton](value: V) extends Auto:
    final def calcWidth(entryCount: Int): Int =
      (entryCount - 1 + value).bitsWidth(false)
    final def encode(idx: Int): BigInt = BigInt(idx + value)
  object StartAt:
    def unapply(using Quotes)(
        tpe: quotes.reflect.TypeRepr
    ): Option[quotes.reflect.TypeRepr] =
      import quotes.reflect.*
      val encodingTpe = TypeRepr.of[StartAt[_]]
      if (tpe <:< encodingTpe)
        val applied =
          tpe.baseType(encodingTpe.typeSymbol).asInstanceOf[AppliedType]
        Some(applied.args.head)
      else None

  abstract class OneHot extends Auto:
    final def calcWidth(entryCount: Int): Int = entryCount
    final def encode(idx: Int): BigInt = BigInt(1) << idx

  abstract class Manual[W <: Int with Singleton](val width: W)
      extends DFEncoding:
    final def calcWidth(entryCount: Int): Int = width
    final def encode(idx: Int): BigInt = value
  object Manual:
    def unapply(using Quotes)(
        tpe: quotes.reflect.TypeRepr
    ): Option[quotes.reflect.TypeRepr] =
      import quotes.reflect.*
      val encodingTpe = TypeRepr.of[Manual[_]]
      if (tpe <:< encodingTpe)
        val applied =
          tpe.baseType(encodingTpe.typeSymbol).asInstanceOf[AppliedType]
        Some(applied.args.head)
      else None

opaque type DFEnum[C <: AnyRef, E] <: DFType.Of[ir.DFEnum] =
  DFType.Of[ir.DFEnum]
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
    ir.DFEnum(name, width, ListMap(entryPairs: _*)).asInstanceOf[DFEnum[C, E]]
