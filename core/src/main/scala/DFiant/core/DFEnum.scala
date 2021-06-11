package DFiant
package core
import scala.quoted.*
import internals.*

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

trait DFEnumCompanion
