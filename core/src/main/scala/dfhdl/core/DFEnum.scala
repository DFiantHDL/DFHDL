package dfhdl.core
import dfhdl.internals.*
import dfhdl.compiler.ir
import scala.quoted.*
import collection.immutable.ListMap
import ir.DFVal.Func.Op as FuncOp
import scala.annotation.targetName

sealed abstract class DFEncoding extends scala.reflect.Enum:
  def calcWidth(entryCount: Int): Int
  def encode(idx: Int): BigInt
  def bigIntValue: BigInt

object DFEncoding:
  sealed trait Auto extends DFEncoding:
    final val bigIntValue: BigInt = encode(ordinal)
  sealed trait ExplicitWidth[W <: Int & Singleton] extends DFEncoding:
    val width: W
  abstract class Default extends StartAt(0)

  abstract class Gray extends Auto:
    final def calcWidth(entryCount: Int): Int =
      (entryCount - 1).bitsWidth(false)
    final def encode(idx: Int): BigInt = BigInt(idx ^ (idx >>> 1))

  abstract class StartAt[V <: Int & Singleton](value: V) extends Auto:
    final def calcWidth(entryCount: Int): Int =
      (entryCount - 1 + value).bitsWidth(false)
    final def encode(idx: Int): BigInt = BigInt(idx + value)

  abstract class OneHot extends Auto:
    final def calcWidth(entryCount: Int): Int = entryCount
    final def encode(idx: Int): BigInt = BigInt(1) << idx

  abstract class Manual[W <: Int & Singleton](val width: W) extends ExplicitWidth[W]:
    val value: DFConstOf[DFUInt[W]]
    final def bigIntValue: BigInt =
      value.asIR match
        case ir.DFVal.Const(dfType = _: ir.DFDecimal, data = data: Option[BigInt] @unchecked) =>
          data.getOrElse(
            throw new IllegalArgumentException(
              "Bubbles are not accepted as enumeration values."
            )
          )
        case _ =>
          throw new IllegalArgumentException(
            "An enumeration value must be a literal constant."
          )

    final def calcWidth(entryCount: Int): Int = width
    final def encode(idx: Int): BigInt = bigIntValue
  end Manual

  abstract class Toggle extends Default, ExplicitWidth[1] derives CanEqual:
    val width: 1 = 1

end DFEncoding

type DFEnum[E <: DFEncoding] = DFType[ir.DFEnum, Args1[E]]
object DFEnum:
  def unapply(using
      Quotes
  )(
      tpe: quotes.reflect.TypeRepr
  ): Option[List[quotes.reflect.TypeRepr]] =
    import quotes.reflect.*
    tpe.asTypeOf[Any] match
      case '[DFEncoding] =>
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
      case _ => None
    end match
  end unapply
  def apply[E <: DFEncoding](enumCompanion: Object): DFEnum[E] =
    val enumClass = classOf[scala.reflect.Enum]
    val enumCompanionCls = enumCompanion.getClass
    val fieldsAsPairs =
      for (
        field <- enumCompanionCls.getDeclaredFields
        if enumClass.isAssignableFrom(field.getType)
      ) yield
        field.setAccessible(true)
        (field.getName, field.get(enumCompanion).asInstanceOf[DFEncoding])
    val name = enumCompanionCls.getSimpleName.replace("$", "")
    val width = fieldsAsPairs.head._2.calcWidth(fieldsAsPairs.size)
    val entryPairs = fieldsAsPairs.zipWithIndex.map { case ((name, entry), idx) =>
      (name, entry.bigIntValue)
    }
    ir.DFEnum(name, width, ListMap(entryPairs*)).asFE[DFEnum[E]]
  end apply

  inline given [E <: DFEncoding]: DFEnum[E] = ${ dfTypeMacro[E] }
  def dfTypeMacro[E <: DFEncoding](using Quotes, Type[E]): Expr[DFEnum[E]] =
    import quotes.reflect.*
    val companionSym = TypeRepr.of[E].typeSymbol.companionModule
    val companionIdent = Ref(companionSym).asExprOf[Object]
    '{ DFEnum[E]($companionIdent) }

  object Val:
    object TC:
      import DFVal.TC
      given DFEnumFromEntry[E <: DFEncoding, RE <: E]: TC[DFEnum[E], RE] with
        type OutP = CONST
        def conv(dfType: DFEnum[E], value: RE)(using DFC): Out =
          DFVal.Const(dfType, Some(value.bigIntValue), named = true)
    object Compare:
      import DFVal.Compare
      given DFEnumCompareEntry[
          E <: DFEncoding,
          RE <: E,
          Op <: FuncOp,
          C <: Boolean
      ]: Compare[DFEnum[E], RE, Op, C] with
        type OutP = CONST
        def conv(dfType: DFEnum[E], arg: RE)(using DFC): Out =
          DFVal.Const(dfType, Some(arg.bigIntValue))
    object Ops:
      given evOpAsDFEnumBinary[
          P,
          L <: DFValTP[DFBoolOrBit, P],
          Comp <: Object,
          E <: DFEncoding
      ](using
          cc: CaseClass.Aux[Comp, DFEncoding, E]
      )(using
          check: E <:< DFEncoding.ExplicitWidth[1],
          dfType: DFEnum[E]
      ): ExactOp2Aux["as", DFC, DFValAny, L, Comp, DFValTP[DFEnum[E], P]] =
        new ExactOp2["as", DFC, DFValAny, L, Comp]:
          type Out = DFValTP[DFEnum[E], P]
          def apply(lhs: L, encodingComp: Comp)(using DFC): Out = trydf {
            DFVal.Alias.AsIs(dfType, lhs)
          }(using dfc, CTName("cast single bit to toggle enum"))
      end evOpAsDFEnumBinary

      // explicitly any ExplicitWidth[1] encoding can be converted to a bool or bit or
      // toggled, and not just Binary.
      extension [P, E <: DFEncoding.ExplicitWidth[1]](lhs: DFValTP[DFEnum[E], P])
        @targetName("boolOfDFEnumBinary")
        def bool(using DFCG): DFValTP[DFBool, P] = trydf {
          DFVal.Alias.AsIs(DFBool, lhs)
        }
        @targetName("bitOfDFEnumBinary")
        def bit(using DFCG): DFValTP[DFBit, P] = trydf {
          DFVal.Alias.AsIs(DFBit, lhs)
        }
        def toggle(using DFCG): DFValTP[DFEnum[E], P] = trydf {
          DFVal.Func(lhs.dfType, FuncOp.unary_!, List(lhs))
        }
      end extension
    end Ops
  end Val
end DFEnum
