package DFiant.core
import DFiant.compiler.ir
import DFiant.internals.*
import DFiant.compiler.ir.DFVal.Modifier

import scala.annotation.{implicitNotFound, targetName}
import scala.quoted.*

trait MyEq
class DFVal[+T <: DFType, +M <: Modifier](val value: ir.DFVal)
    extends AnyVal
    with DFMember[ir.DFVal]:
  def ==[R](that: R): DFBool <> VAL = ???
object DFVal:
  final val Modifier = DFiant.compiler.ir.DFVal.Modifier
  export DFBits.Val.Conversions.given
  export DFDecimal.Val.Conversions.*
  export CompanionsDFVal.Conversions.*
  export CompanionsDFVal.Extensions.*
  val Const = CompanionsDFVal.Const
  val Dcl = CompanionsDFVal.Dcl
  val Func = CompanionsDFVal.Func
  val Alias = CompanionsDFVal.Alias
  val TC = CompanionsDFVal.TC
  type TC[T <: DFType, -R] = CompanionsDFVal.TC[T, R]
  val Equals = CompanionsDFVal.Equals
  type Equals[T <: DFType, -V, NE <: Boolean] = CompanionsDFVal.Equals[T, V, NE]
  val Ops = CompanionsDFVal.Ops
end DFVal

type DFValAny = DFVal[DFType, Modifier]
type DFValOf[+T <: DFType] = DFVal[T, Modifier]
type DFVarOf[+T <: DFType] = DFVal[T, Modifier.Assignable]
type DFPortOf[+T <: DFType] = DFVal[T, Modifier.Port]

val IN = Modifier.IN
val OUT = Modifier.OUT
val INOUT = Modifier.INOUT
val VAR = Modifier.VAR
type VAL = Modifier.VAL
type VAR = Modifier.VAR.type
type IN = Modifier.IN.type
type OUT = Modifier.OUT.type
trait TOKEN
type <>[T <: DFType, M] = M match
  case VAL   => DFValOf[T]
  case VAR   => DFVarOf[T]
  case IN    => DFPortOf[T]
  case OUT   => DFPortOf[T]
  case TOKEN => DFToken.Of[T]

extension (dfVal: ir.DFVal)
  def asVal[T <: DFType, M <: Modifier]: DFVal[T, M] = DFVal[T, M](dfVal)
  def asValOf[T <: DFType]: DFValOf[T] = DFVal[T, Modifier](dfVal)
  def asValAny: DFValAny = DFVal[DFType, Modifier](dfVal)
  def asVarOf[T <: DFType]: DFVarOf[T] = DFVal[T, Modifier.Assignable](dfVal)
  def asPortOf[T <: DFType]: DFPortOf[T] = DFVal[T, Modifier.Port](dfVal)

private object CompanionsDFVal:
  object Extensions:
    extension [T <: DFType, M <: Modifier](dfVal: DFVal[T, M])
      def init(tokenValues: DFToken.Value[T]*)(using dfc: DFC): DFVal[T, M] =
        import dfc.getSet
        val tokens =
          tokenValues.map(tv => tv(dfVal.dfType).asIR)
        assert(
          dfVal.asIR.isAnonymous,
          s"Cannot initialize a named value ${dfVal.asIR.getFullName}. Initialization is only supported at the declaration of the value."
        )
        dfVal.asIR.tag(ir.ExternalInit(tokens)).asVal[T, M]
  end Extensions

  object Conversions:
    implicit inline def DFValConversion[T <: DFType, R](
        inline from: R
    )(using dfType: T, es: Exact.Summon[from.type])(using
        tc: CompanionsDFVal.TC[T, es.Out]
    ): DFValOf[T] = tc(dfType, es(from))

  object Const:
    def apply[T <: DFType](token: DFToken.Of[T], named: Boolean = false)(using
        DFC
    ): DFValOf[T] =
      val meta = if (named) dfc.getMeta else dfc.getMeta.anonymize
      ir.DFVal
        .Const(token.asIR, dfc.owner.ref, meta, ir.DFTags.empty)
        .addMember
        .asValOf[T]

  object Dcl:
    def apply[T <: DFType, M <: Modifier](dfType: T, modifier: M)(using
        DFC
    ): DFVal[T, M] =
      ir.DFVal
        .Dcl(
          dfType.asIR,
          modifier,
          dfc.owner.ref,
          dfc.getMeta,
          ir.DFTags.empty
        )
        .addMember
        .asVal[T, M]
  end Dcl

  object Func:
    def apply[T <: DFType](
        dfType: T,
        op: ir.DFVal.Func.Op,
        args: List[DFValAny]
    )(using DFC): DFValOf[T] =
      lazy val func: ir.DFVal = ir.DFVal.Func(
        dfType.asIR,
        op,
        args.map(_.asIR.refTW(func)),
        dfc.owner.ref,
        dfc.getMeta,
        ir.DFTags.empty
      )
      func.addMember.asValOf[T]
  end Func

  object Alias:
    object AsIs:
      def apply[AT <: DFType, VT <: DFType, M <: Modifier](
          aliasType: AT,
          relVal: DFVal[VT, M],
          tokenFunc: DFToken.Of[VT] => DFToken.Of[AT]
      )(using DFC): DFVal[AT, M] =
        relVal.asIR match
          //anonymous constant are replace by a different constant
          //after its token value was converted according to the alias
          case const: ir.DFVal.Const if const.isAnonymous =>
            val updatedToken = tokenFunc(const.token.asTokenOf[VT])
            Const(updatedToken).asIR.asVal[AT, M]
          //named constants or other non-constant values are referenced
          //in a new alias construct
          case _ =>
            lazy val alias: ir.DFVal =
              ir.DFVal.Alias.AsIs(
                aliasType.asIR,
                relVal.asIR.refTW(alias),
                dfc.owner.ref,
                dfc.getMeta,
                ir.DFTags.empty
              )
            alias.addMember.asVal[AT, M]
      end apply
    end AsIs
    object ApplyRange:
      def apply[W <: Int, M <: Modifier, H <: Int, L <: Int](
          relVal: DFVal[DFBits[W], M],
          relBitHigh: Inlined[H],
          relBitLow: Inlined[L]
      )(using DFC): DFVal[DFBits[H - L + 1], M] =
        lazy val alias: ir.DFVal =
          ir.DFVal.Alias.ApplyRange(
            relVal.asIR.refTW(alias),
            relBitHigh,
            relBitLow,
            dfc.owner.ref,
            dfc.getMeta,
            ir.DFTags.empty
          )
        alias.addMember.asVal[DFBits[H - L + 1], M]
      end apply
    end ApplyRange
    object ApplyIdx:
      def apply[W <: Int, M <: Modifier, IW <: Int](
          relVal: DFVal[DFBits[W], M],
          relIdx: DFUInt[IW] <> VAL
      )(using DFC): DFVal[DFBit, M] =
        lazy val alias: ir.DFVal =
          ir.DFVal.Alias.ApplyIdx(
            ir.DFBit,
            relVal.asIR.refTW(alias),
            relIdx.asIR.refTW(alias),
            dfc.owner.ref,
            dfc.getMeta,
            ir.DFTags.empty
          )
        alias.addMember.asVal[DFBit, M]
      end apply
      def apply[W <: Int, M <: Modifier, I <: Int](
          relVal: DFVal[DFBits[W], M],
          relIdx: Inlined[I]
      )(using DFC): DFVal[DFBit, M] =
        ???
    end ApplyIdx
  end Alias

  @implicitNotFound(
    "Unsupported argument value ${R} for dataflow receiver type ${T}"
  )
  trait TC[T <: DFType, -R] extends GeneralTC[T, R, DFValAny]:
    type Out = DFValOf[T]
  trait TCLP:
    //Accept any token value, according to a token type class
    transparent inline given [T <: DFType, R](using
        tokenTC: DFToken.TC[T, R],
        dfc: DFC
    ): TC[T, R] =
      new TC[T, R]:
        type TType = T
        def apply(dfType: T, value: R): DFValOf[T] =
          Const(tokenTC(dfType, value))
  object TC extends TCLP:
    export DFBits.Val.TC.given
    export DFDecimal.Val.TC.given
    export DFTuple.Val.TC.given
    //Accept any dataflow value of the same type
    transparent inline given [T <: DFType]: TC[T, DFValOf[T]] =
      new TC[T, DFValOf[T]]:
        type TType = T
        def apply(dfType: T, value: DFValOf[T]): DFValOf[T] =
          val updated = (dfType.asIR, value.asIR.dfType) match
            case (_: ir.DFBoolOrBit, _: ir.DFBoolOrBit) => value
            case (_: ir.DFBits, _: ir.DFBits) =>
              DFBits.Val.TC(
                dfType.asIR.asFE[DFBits[Int]],
                value.asIR.asValOf[DFBits[Int]]
              )
            case (_: ir.DFDecimal, _: ir.DFDecimal) =>
              DFDecimal.Val.TC(
                dfType.asIR.asFE[DFDecimal[Boolean, Int, Int]],
                value.asIR.asValOf[DFDecimal[Boolean, Int, Int]]
              )
            case _ =>
              throw new IllegalArgumentException(
                s"Unsupported argument value ${value} for dataflow receiver type ${dfType}"
              )
          updated.asIR.asValOf[T]
        end apply
  end TC

  @implicitNotFound("Cannot compare dataflow value of ${T} with value of ${V}")
  trait Equals[T <: DFType, -V, NE <: Boolean]:
    def apply(dfVal: DFValOf[T], arg: V): DFValOf[DFBool]
  object Equals

//  object Conversions:
//    implicit transparent inline def fromArg[T <: DFType, R](
//        inline arg: R
//    ): DFValOf[T] = ${ fromArgMacro[T]('arg) }

  object Ops:
    extension [T <: DFType, M <: Modifier](dfVal: DFVal[T, M])
      def bits(using w: Width[T])(using DFC): DFValOf[DFBits[w.Out]] =
        import DFToken.Ops.{bits => bitsDFToken}
        DFVal.Alias.AsIs(DFBits(dfVal.width), dfVal, _.bitsDFToken)
  end Ops
end CompanionsDFVal

object DFValNI:
  //TODO: Delete if no use eventually
  inline def initTokens[T <: DFType](
      dfType: T,
      inline tokenValues: Any*
  ): Seq[DFToken] =
    ${
      initTokensMacro[T]('dfType, 'tokenValues)
    }
  def initTokensMacro[T <: DFType](
      dfType: Expr[T],
      tokenValues: Expr[Seq[Any]]
  )(using
      Quotes,
      Type[T]
  ): Expr[Seq[DFToken]] =
    import quotes.reflect.*
    val Varargs(args) = tokenValues
    val valueOfTpe = TypeRepr.of[ValueOf]
    val argShowedExprs = args.map { case '{ $arg: tp } =>
      arg.asTerm match
        case Literal(const) =>
          val tpe = valueOfTpe
            .appliedTo(ConstantType(const))
            .asType
            .asInstanceOf[Type[Any]]
          '{
            compiletime.summonInline[DFToken.TC[T, tpe.Underlying]](
              $dfType,
              ValueOf($arg)
            )
          }
        case _ =>
          '{ compiletime.summonInline[DFToken.TC[T, tp]]($dfType, $arg) }
    }
    '{ Seq(${ Varargs(argShowedExprs) }*) }
  end initTokensMacro
end DFValNI

extension [T <: DFType](dfVar: DFVarOf[T])
  def assign[R <: DFType](rhs: DFValOf[R])(using DFC): Unit =
    DFNet(dfVar.asIR, DFNet.Op.Assignment, rhs.asIR)

object DFVarOps:
  extension [T <: DFType](dfVar: DFVarOf[T])
    def :=[R](rhs: Exact[R])(using tc: DFVal.TC[T, R], dfc: DFC): Unit =
      dfVar.assign(tc(dfVar.dfType, rhs))
