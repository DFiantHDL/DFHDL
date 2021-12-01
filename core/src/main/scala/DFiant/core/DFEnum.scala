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

  abstract class OneHot extends Auto:
    final def calcWidth(entryCount: Int): Int = entryCount
    final def encode(idx: Int): BigInt = BigInt(1) << idx

  abstract class Manual[W <: Int with Singleton](val width: W)
      extends DFEncoding:
    final def calcWidth(entryCount: Int): Int = width
    final def encode(idx: Int): BigInt = value
end DFEncoding

type DFEnum[E <: DFEncoding] = DFType[ir.DFEnum, Args1[E]]
object DFEnum:
  def unapply(using Quotes)(
      tpe: quotes.reflect.TypeRepr
  ): Option[List[quotes.reflect.TypeRepr]] =
    import quotes.reflect.*
    val enumTpe = TypeRepr.of[scala.reflect.Enum]
    val sym = tpe.typeSymbol
    val symCls = sym.companionClass
    val symMdl = sym.companionModule
    if (sym.flags.is(Flags.Enum) || symCls.flags.is(Flags.Enum))
      Some(
        symMdl.declaredFields.view
          .map(f => tpe.memberType(f))
          .filter(_ <:< enumTpe)
          .toList
      )
    else None
  end unapply
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

  inline given [E <: DFEncoding]: DFEnum[E] = ${ dfTypeMacro[E] }
  def dfTypeMacro[E <: DFEncoding](using Quotes, Type[E]): Expr[DFEnum[E]] =
    import quotes.reflect.*
    val companionSym = TypeRepr.of[E].typeSymbol.companionModule
    val companionIdent = Ref(companionSym).asExprOf[AnyRef]
    '{ DFEnum[E]($companionIdent) }

  type Token[E <: DFEncoding] = DFToken[DFEnum[E]]
  object Token:
    extension [E <: DFEncoding](token: Token[E])
      def data: Option[BigInt] =
        token.asIR.data.asInstanceOf[Option[BigInt]]
    def apply[E <: DFEncoding, RE <: E](
        dfType: DFEnum[E],
        entry: RE
    ): Token[E] =
      ir.DFToken(dfType.asIR)(Some(entry.value)).asTokenOf[DFEnum[E]]

    object TC:
      import DFToken.TC
      given DFEnumTokenFromEntry[E <: DFEncoding, RE <: E]: TC[DFEnum[E], RE] =
        (dfType: DFEnum[E], value: RE) => Token[E, RE](dfType, value)

    object Compare:
      import DFToken.Compare
      given DFEnumCompareEntry[
          E <: DFEncoding,
          RE <: E,
          Op <: FuncOp,
          C <: Boolean
      ](using
          op: ValueOf[Op]
      ): Compare[DFEnum[E], RE, Op, C] with
        def conv(dfType: DFEnum[E], arg: RE): DFEnum[E] <> TOKEN =
          Token[E, RE](dfType, arg)
  end Token
  object Val:
    object TC:
      import DFVal.TC
      given DFEnumFromEntry[E <: DFEncoding, RE <: E](using
          dfc: DFC
      ): TC[DFEnum[E], RE] with
        def apply(dfType: DFEnum[E], value: RE): DFValOf[DFEnum[E]] =
          DFVal.Const(Token[E, RE](dfType, value))
    object Compare:
      import DFVal.Compare
      given DFEnumCompareEntry[
          E <: DFEncoding,
          RE <: E,
          Op <: FuncOp,
          C <: Boolean
      ]: Compare[DFEnum[E], RE, Op, C] with
        def conv(dfType: DFEnum[E], arg: RE)(using DFC): DFEnum[E] <> VAL =
          DFVal.Const(Token[E, RE](dfType, arg))
  end Val
end DFEnum
