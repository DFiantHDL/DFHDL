package DFiant.core
import DFiant.compiler.ir
import DFiant.internals.*

import scala.annotation.{implicitNotFound, targetName}
import scala.quoted.*

//TODO: simplify after https://github.com/lampepfl/dotty/issues/13120 is fixed
opaque type DFVal[+T <: DFType, +M <: DFVal.Modifier] <: DFMember.Of[
  DFiant.compiler.ir.DFVal
] =
  DFMember.Of[DFiant.compiler.ir.DFVal]
type DFValOf[+T <: DFType] = DFVal[T, DFVal.Modifier]
type DFVarOf[+T <: DFType] = DFVal[T, DFVal.Modifier.Assignable]
type DFPortOf[+T <: DFType] = DFVal[T, DFVal.Modifier.Port]

type VAL = DFVal.Modifier.VAL
type VAR = DFVal.Modifier.VAR.type
type IN = DFVal.Modifier.IN.type
type OUT = DFVal.Modifier.OUT.type
trait TOKEN
type <>[T <: DFType, M] = M match
  case VAL   => DFValOf[T]
  case VAR   => DFVarOf[T]
  case IN    => DFPortOf[T]
  case OUT   => DFPortOf[T]
  case TOKEN => DFToken.Of[T]

extension (dfVal: ir.DFVal)
  def asValOf[T <: DFType]: DFValOf[T] = dfVal.asInstanceOf[DFValOf[T]]
  def asVarOf[T <: DFType]: DFVarOf[T] = dfVal.asInstanceOf[DFVarOf[T]]
  def asPortOf[T <: DFType]: DFPortOf[T] = dfVal.asInstanceOf[DFPortOf[T]]

object DFVal:
  export DFiant.compiler.ir.DFVal.Modifier

  extension [T <: DFType, M <: Modifier](dfVal: DFVal[T, M])
    def dfType: T = dfVal.asIR.dfType.asInstanceOf[T]
    def width(using w: Width[T]): Inlined.Int[w.Out] =
      Inlined.Int.forced[w.Out](dfVal.asIR.dfType.width)
    def init(tokenValues: DFToken.Value[T]*)(using dfc: DFC): DFVal[T, M] =
      import dfc.getSet
      val tokens =
        tokenValues.map(tv => tv(dfVal.dfType).asIR)
      assert(
        dfVal.asIR.isAnonymous,
        s"Cannot initialize a named value ${dfVal.asIR.getFullName}. Initialization is only supported at the declaration of the value."
      )
      dfVal.asIR.tag(ir.ExternalInit(tokens)).asFE[T, M]

  object Const:
    def apply[T <: DFType](token: DFToken, named: Boolean = false)(using
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
        .asFE[T, M]

  object Alias:
    object AsIs:
      def apply[AT <: DFType, VT <: DFType, M <: Modifier](
          aliasType: AT,
          relVal: DFVal[VT, M]
      )(using DFC): DFVal[AT, M] =
        lazy val alias: ir.DFVal =
          ir.DFVal.Alias.AsIs(
            aliasType.asIR,
            relVal.asIR.refTW(alias),
            dfc.owner.ref,
            dfc.getMeta,
            ir.DFTags.empty
          )
        alias.addMember.asFE[AT, M]

  @implicitNotFound(
    "Unsupported argument value ${R} for dataflow receiver type ${T}"
  )
  trait TC[T <: DFType, R]:
    type Out <: DFType
    def apply(dfType: T, value: R): DFValOf[Out]
  object TC:
    export DFBits.DFValTC.given
    export DFTuple.DFValTC.given
    //Accept any dataflow value of the same type
    transparent inline given [T <: DFType]: TC[T, DFValOf[T]] =
      new TC[T, DFValOf[T]]:
        type Out = T
        def apply(dfType: T, value: DFValOf[T]): DFValOf[T] =
          val updated = (dfType.asIR, value.asIR.dfType) match
            case (_: ir.DFBoolOrBit, _: ir.DFBoolOrBit) => value
            case (_: ir.DFBits, _: ir.DFBits) =>
              summon[TC[DFBits[Int], DFBits[Int] <> VAL]](
                dfType.asIR.asFE[DFBits[Int]],
                value.asIR.asValOf[DFBits[Int]]
              )
            case _ =>
              throw new IllegalArgumentException(
                s"Unsupported argument value ${value} for dataflow receiver type ${dfType}"
              )
          updated.asIR.asValOf[T]
    //Accept any token value, according to a token type class
    transparent inline given [T <: DFType, R](using
        tokenTC: DFToken.TC[T, R],
        dfc: DFC
    ): TC[T, R] =
      new TC[T, R]:
        type Out = T
        def apply(dfType: T, value: R): DFValOf[T] =
          Const(tokenTC(dfType, value))
  end TC

  object Ops:
    extension [T <: DFType, M <: DFVal.Modifier](dfVal: DFVal[T, M])
      def bits(using w: Width[T])(using DFC): DFValOf[DFBits[w.Out]] =
        DFVal.Alias.AsIs(DFBits(dfVal.width), dfVal)
  end Ops
end DFVal

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
      arg.asTerm match {
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
    }
    '{ Seq(${ Varargs(argShowedExprs) }*) }

extension [T <: DFType](dfVar: DFVarOf[T])
  def assign[R <: DFType](rhs: DFValOf[R])(using DFC): Unit =
    DFNet(dfVar.asIR, DFNet.Op.Assignment, rhs.asIR)

object DFVarOps:
  extension [T <: DFType](dfVar: DFVarOf[T])
    def :=[R](rhs: Exact[R])(using tc: DFVal.TC[T, R], dfc: DFC): Unit =
      dfVar.assign(tc(dfVar.dfType, rhs))
