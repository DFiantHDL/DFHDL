package DFiant
package core
import DFiant.compiler.ir
import scala.quoted.*
import internals.*
import collection.immutable.ListMap
import ir.DFVal.Func.Op as FuncOp

sealed trait DFEncoding extends scala.reflect.Enum:
//  def unapply[E](arg: DFValOf[DFEnum[E]])(using DFC): Boolean = false
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
end DFEncoding

type DFEnum[E <: DFEncoding] = OpaqueDFEnum.DFEnum[E]
val DFEnum = OpaqueDFEnum.DFEnum

private object OpaqueDFEnum:
  type DFEnum[E <: DFEncoding] =
    DFType[ir.DFEnum, Args1[E]]
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
    def apply[E <: DFEncoding](enumCompanion: AnyRef): DFEnum[E] =
      val enumClass = classOf[scala.reflect.Enum]
      val enumCompanionCls = enumCompanion.getClass
      val fieldsAsPairs = for (
        field <- enumCompanionCls.getDeclaredFields
        if enumClass.isAssignableFrom(field.getType)
      ) yield
        field.setAccessible(true)
        (field.getName, field.get(enumCompanion).asInstanceOf[DFEncoding])
      val name = enumCompanionCls.getSimpleName.replace("$", "")
      val width = fieldsAsPairs.head._2.calcWidth(fieldsAsPairs.size)
      val entryPairs = fieldsAsPairs.zipWithIndex.map {
        case ((name, entry), idx) => (name, entry.value)
      }
      ir.DFEnum(name, width, ListMap(entryPairs: _*)).asFE[DFEnum[E]]
    end apply

    type Token[E <: DFEncoding] = DFToken[DFEnum[E]]
    object Token:
      extension [E <: DFEncoding](token: Token[E])
        def data: Option[BigInt] =
          token.asIR.data.asInstanceOf[Option[BigInt]]
      def apply[E <: DFEncoding, RE <: E](
          dfType: DFEnum[E],
          entry: RE
      ): Token[E] =
        ir.DFToken(dfType.asIR, Some(entry.value)).asTokenOf[DFEnum[E]]

      object TC:
        import DFToken.TC
        given DFEnumTokenFromEntry[E <: DFEncoding, RE <: E]
            : TC[DFEnum[E], RE] =
          (dfType: DFEnum[E], value: RE) => Token[E, RE](dfType, value)
        given DFEnumTokenFromToken[E <: DFEncoding]
            : TC[DFEnum[E], DFToken[DFEnum[E]]] =
          (dfType: DFEnum[E], value: DFToken[DFEnum[E]]) => value

      object Compare:
        import DFToken.Compare
        given DFEnumCompareEntry[
            E <: DFEncoding,
            R,
            Op <: FuncOp,
            C <: Boolean
        ](using
            tc: DFToken.TC[DFEnum[E], R],
            op: ValueOf[Op]
        ): Compare[DFEnum[E], R, Op, C] with
          def apply(token: DFToken[DFEnum[E]], arg: R): DFToken[DFBool] =
            val tokenArg = tc(token.dfType, arg)
            val outData = (token.data, tokenArg.data) match
              case (Some(l), Some(r)) =>
                op.value match
                  case FuncOp.=== => Some(l == r)
                  case FuncOp.=!= => Some(l != r)
                  case _ => throw new IllegalArgumentException("Unsupported Op")
              case _ => None
            DFBoolOrBit.Token(DFBool, outData)
        end DFEnumCompareEntry
      end Compare
    end Token
    object Val:
      object TC:
        import DFVal.TC
        given DFEnumFromTokenTC[E <: DFEncoding, R](using
            tc: DFToken.TC[DFEnum[E], R],
            dfc: DFC
        ): TC[DFEnum[E], R] with
          def apply(dfType: DFEnum[E], value: R): DFValOf[DFEnum[E]] =
            DFVal.Const(tc(dfType, value))
      object Compare:
        import DFVal.Compare
  end DFEnum
end OpaqueDFEnum
