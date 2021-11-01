package DFiant.core
import DFiant.compiler.ir
import DFiant.internals.*
import DFiant.compiler.ir.DFVal.Modifier
import ir.DFVal.Func.{Op => FuncOp}
import scala.annotation.unchecked.uncheckedVariance
import scala.annotation.{implicitNotFound, targetName}
import scala.quoted.*

final class DFVal[+T <: DFTypeAny, +M <: Modifier](val value: ir.DFVal)
    extends AnyVal
    with DFMember[ir.DFVal]:
  transparent inline def ==[R](
      inline that: R
  )(using DFC): DFBool <> VAL = ${
    DFVal.equalityMacro[T, R, FuncOp.===.type]('this, 'that)
  }
  transparent inline def !=[R](
      inline that: R
  )(using DFC): DFBool <> VAL = ${
    DFVal.equalityMacro[T, R, FuncOp.=!=.type]('this, 'that)
  }
end DFVal

object DFVal:
  def equalityMacro[T <: DFTypeAny, R, Op <: FuncOp](
      dfVal: Expr[DFValOf[T]],
      arg: Expr[R]
  )(using Quotes, Type[T], Type[R], Type[Op]): Expr[DFValOf[DFBool]] =
    import quotes.reflect.*
    val exact = arg.asTerm.exactTerm
    val exactExpr = exact.asExpr
    val exactType = exact.tpe.asTypeOf[Any]
    '{
      val c = compiletime.summonInline[
        DFVal.Compare[T, exactType.Underlying, Op, false]
      ]
      c($dfVal, $exactExpr)(using compiletime.summonInline[DFC])
    }
  end equalityMacro

  //Enabling equality with Int, Boolean, and Tuples.
  //just to give a better error message via the compiler plugins.
  //See the method `rejectBadEquals` in `MetaContextGenPhase.scala`
  given [T <: DFTypeAny, M <: Modifier]: CanEqual[Int, DFVal[T, M]] =
    CanEqual.derived
  given [T <: DFTypeAny, M <: Modifier]: CanEqual[Boolean, DFVal[T, M]] =
    CanEqual.derived
  given [T <: DFTypeAny, M <: Modifier]: CanEqual[Tuple, DFVal[T, M]] =
    CanEqual.derived

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
  type TC[T <: DFTypeAny, -R] = CompanionsDFVal.TC[T, R]
  val Compare = CompanionsDFVal.Compare
  type Compare[T <: DFTypeAny, -V, Op <: FuncOp, C <: Boolean] =
    CompanionsDFVal.Compare[T, V, Op, C]
  val Ops = CompanionsDFVal.Ops
end DFVal

type DFValAny = DFVal[DFTypeAny, Modifier]
type DFValOf[+T <: DFTypeAny] = DFVal[T, Modifier]
type DFVarOf[+T <: DFTypeAny] = DFVal[T, Modifier.Assignable]
type DFPortOf[+T <: DFTypeAny] = DFVal[T, Modifier.Port]

val IN = Modifier.IN
val OUT = Modifier.OUT
val INOUT = Modifier.INOUT
val VAR = Modifier.VAR
type VAL = Modifier.VAL
type VAR = Modifier.VAR.type
type IN = Modifier.IN.type
type OUT = Modifier.OUT.type
sealed trait TOKEN
type <>[T <: DFTypeAny | DFEncoding, M] = M match
  case VAL =>
    T match
      case DFTypeAny  => DFValOf[T]
      case DFEncoding => DFValOf[DFEnum[T]]
  case VAR =>
    T match
      case DFTypeAny  => DFVarOf[T]
      case DFEncoding => DFVarOf[DFEnum[T]]
  case IN | OUT =>
    T match
      case DFTypeAny  => DFPortOf[T]
      case DFEncoding => DFPortOf[DFEnum[T]]
  case TOKEN =>
    T match
      case DFTypeAny  => DFToken[T]
      case DFEncoding => DFToken[DFEnum[T]]

extension (dfVal: ir.DFVal)
  def asVal[T <: DFTypeAny, M <: Modifier]: DFVal[T, M] = DFVal[T, M](dfVal)
  def asValOf[T <: DFTypeAny]: DFValOf[T] = DFVal[T, Modifier](dfVal)
  def asValAny: DFValAny = DFVal[DFTypeAny, Modifier](dfVal)
  def asVarOf[T <: DFTypeAny]: DFVarOf[T] = DFVal[T, Modifier.Assignable](dfVal)
  def asPortOf[T <: DFTypeAny]: DFPortOf[T] = DFVal[T, Modifier.Port](dfVal)

private object CompanionsDFVal:
  object Extensions:
    extension [T <: DFTypeAny, M <: Modifier](dfVal: DFVal[T, M])
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
    implicit inline def DFValConversion[T <: DFTypeAny, R](
        inline from: R
    )(using dfType: T, es: Exact.Summon[R, from.type])(using
        tc: CompanionsDFVal.TC[T, es.Out]
    ): DFValOf[T] = tc(dfType, es(from))

  object Const:
    def apply[T <: DFTypeAny](token: DFToken[T], named: Boolean = false)(using
        DFC
    ): DFValOf[T] =
      val meta = if (named) dfc.getMeta else dfc.getMeta.anonymize
      ir.DFVal
        .Const(token.asIR, dfc.owner.ref, meta, ir.DFTags.empty)
        .addMember
        .asValOf[T]

  object Dcl:
    def apply[T <: DFTypeAny, M <: Modifier](dfType: T, modifier: M)(using
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
    export ir.DFVal.Func.Op
    def apply[T <: DFTypeAny](
        dfType: T,
        op: FuncOp,
        args: List[DFValAny]
    )(using DFC): DFValOf[T] = apply(dfType, op, args.map(_.asIR))
    @targetName("applyFromIR")
    def apply[T <: DFTypeAny](
        dfType: T,
        op: FuncOp,
        args: List[ir.DFVal]
    )(using DFC): DFValOf[T] =
      lazy val func: ir.DFVal = ir.DFVal.Func(
        dfType.asIR,
        op,
        args.map(_.refTW(func)),
        dfc.owner.ref,
        dfc.getMeta,
        ir.DFTags.empty
      )
      func.addMember.asValOf[T]
    end apply
  end Func

  object Alias:
    object AsIs:
      def apply[AT <: DFTypeAny, VT <: DFTypeAny, M <: Modifier](
          aliasType: AT,
          relVal: DFVal[VT, M],
          tokenFunc: DFToken[VT] => DFToken[AT]
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
  trait TC[T <: DFTypeAny, -R] extends GeneralTC[T, R, DFValAny]:
    type Out = DFValOf[T]
  trait TCLP:
    //Accept any token value, according to a token type class
    transparent inline given [T <: DFTypeAny, R](using
        tokenTC: DFToken.TC[T, R],
        dfc: DFC
    ): TC[T, R] =
      new TC[T, R]:
        type TType = T
        def apply(dfType: T, value: R): DFValOf[T] =
          Const(tokenTC(dfType, value))
  object TC extends TCLP:
    export DFBoolOrBit.Val.TC.given
    export DFBits.Val.TC.given
    export DFDecimal.Val.TC.given
    export DFEnum.Val.TC.given
    export DFTuple.Val.TC.given
    //Accept any dataflow value of the same type
    transparent inline given [T <: DFTypeAny]: TC[T, DFValOf[T]] =
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
  trait Compare[T <: DFTypeAny, -V, Op <: FuncOp, C <: Boolean]:
    final protected def func(arg1: DFValAny, arg2: DFValAny)(using
        DFC,
        ValueOf[Op],
        ValueOf[C]
    ): DFValOf[DFBool] =
      val list = if (valueOf[C]) List(arg2, arg1) else List(arg1, arg2)
      DFVal.Func(DFBool, valueOf[Op], list)
    def apply(dfVal: DFValOf[T], arg: V)(using DFC): DFValOf[DFBool]
  object Compare:
    export DFBoolOrBit.Val.Compare.given
    export DFBits.Val.Compare.given
    export DFDecimal.Val.Compare.given
    export DFEnum.Val.Compare.given

//  object Conversions:
//    implicit transparent inline def fromArg[T <: DFTypeAny, R](
//        inline arg: R
//    ): DFValOf[T] = ${ fromArgMacro[T]('arg) }

  object Ops:
    extension [T <: DFTypeAny, M <: Modifier](dfVal: DFVal[T, M])
      def bits(using w: Width[T])(using DFC): DFValOf[DFBits[w.Out]] =
        import DFToken.Ops.{bits => bitsDFToken}
        DFVal.Alias.AsIs(DFBits(dfVal.width), dfVal, _.bitsDFToken)
  end Ops
end CompanionsDFVal

object DFValNI:
  //TODO: Delete if no use eventually
  inline def initTokens[T <: DFTypeAny](
      dfType: T,
      inline tokenValues: Any*
  ): Seq[DFTokenAny] =
    ${
      initTokensMacro[T]('dfType, 'tokenValues)
    }
  def initTokensMacro[T <: DFTypeAny](
      dfType: Expr[T],
      tokenValues: Expr[Seq[Any]]
  )(using
      Quotes,
      Type[T]
  ): Expr[Seq[DFTokenAny]] =
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

extension [T <: DFTypeAny](dfVar: DFVarOf[T])
  def assign[R <: DFTypeAny](rhs: DFValOf[R])(using DFC): Unit =
    DFNet(dfVar.asIR, DFNet.Op.Assignment, rhs.asIR)

object DFVarOps:
  extension [T <: DFTypeAny](dfVar: DFVarOf[T])
    def :=[R](rhs: Exact[R])(using tc: DFVal.TC[T, R], dfc: DFC): Unit =
      dfVar.assign(tc(dfVar.dfType, rhs))
