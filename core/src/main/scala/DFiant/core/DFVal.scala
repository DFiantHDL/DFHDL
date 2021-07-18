package DFiant.core
import DFiant.compiler.ir
import DFiant.internals.*

import scala.annotation.{implicitNotFound, targetName}
import scala.quoted.*

opaque type DFVal[+T <: DFType, +M <: DFVal.Modifier] = ir.DFVal
type DFValOf[+T <: DFType] = DFVal[T, DFVal.Modifier.VAL]
type DFVarOf[+T <: DFType] = DFVal[T, DFVal.Modifier.Assignable]
type DFPortOf[+T <: DFType] = DFVal[T, DFVal.Modifier.Port]

extension (dfVal: ir.DFVal)
  def asValOf[T <: DFType]: DFValOf[T] = dfVal
  def asVarOf[T <: DFType]: DFVarOf[T] = dfVal
  def asPortOf[T <: DFType]: DFPortOf[T] = dfVal

object DFVal:
  export ir.DFVal.Modifier

  extension [T <: DFType, M <: Modifier](dfVal: DFVal[T, M])
    def dfType: T = dfVal.asIR.dfType.asInstanceOf[T]
    def width(using w: Width[T]): Inlined.Int[w.Out] =
      Inlined.Int.forced[w.Out](dfVal.asIR.dfType.width)
    def asIR: ir.DFVal = dfVal

  object Const:
    def apply[T <: DFType](token: DFToken, named: Boolean = false)(using
        DFC
    ): DFValOf[T] =
      val meta = if (named) dfc.getMeta else dfc.getMeta.anonymize
      ir.DFVal
        .Const(token.asIR, dfc.owner.ref, meta, ir.DFTags.empty)
        .addMember

  object Dcl:
    def apply[T <: DFType, M <: Modifier](dfType: T, modifier: M)(using
        DFC
    ): DFValNI[T, M] =
      ir.DFVal
        .Dcl(
          dfType.asIR,
          modifier,
          None,
          dfc.owner.ref,
          dfc.getMeta,
          ir.DFTags.empty
        )
        .addMember
    def apply[T <: DFType, M <: Modifier](
        dfVal: DFValNI[T, M],
        init: Seq[DFToken]
    )(using DFC): DFVal[T, M] =
      dfVal
        .asInstanceOf[ir.DFVal.Dcl]
        .copy(externalInit = Some(init.map(_.asIR)))

  @implicitNotFound("Unsupported argument value ${R} for dataflow type ${T}")
  trait TC[T <: DFType, -R]:
    type Out <: DFType
    def apply(dfType: T, value: R): DFValOf[Out]
  object TC:
    //Accept any dataflow value of the same type
    transparent inline given [T <: DFType]: TC[T, DFValOf[T]] =
      new TC[T, DFValOf[T]]:
        type Out = T
        def apply(dfType: T, value: DFValOf[T]): DFValOf[T] = value
    //Accept any token value, according to a token type class
    transparent inline given [T <: DFType, R](using
        tokenTC: DFToken.TC[T, R],
        dfc: DFC
    ): TC[T, R] =
      new TC[T, R]:
        type Out = T
        def apply(dfType: T, value: R): DFValOf[T] =
          Const(tokenTC(dfType, value))
end DFVal

opaque type DFValNI[+T <: DFType, +M <: DFVal.Modifier] <: DFVal[T, M] =
  DFVal[T, M]
object DFValNI:
  extension [T <: DFType, M <: DFVal.Modifier](dcl: DFValNI[T, M])
    private def asIR: ir.DFVal.Dcl = dcl.asInstanceOf[ir.DFVal.Dcl]
    def init(tokenValues: DFToken.Value[T]*)(using DFC): DFVal[T, M] =
      val tokens = tokenValues.map(tv => tv(asIR.dfType.asInstanceOf[T]).asIR)
      val updated = asIR.copy(externalInit = Some(tokens), meta = dfc.getMeta)
      if (dcl.isAnonymous)
        asIR.replaceMemberWith(updated)
      else
        updated.addMember
      updated.asInstanceOf[DFVal[T, M]]

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
    import DFVal.asIR
    DFNet(dfVar.asIR, DFNet.Op.Assignment, rhs.asIR)

object DFVarOps:
  extension [T <: DFType](dfVar: DFVarOf[T])
    def :=[R](rhs: Exact[R])(using tc: DFVal.TC[T, R], dfc: DFC): Unit =
      dfVar.assign(tc(dfVar.dfType, rhs))
  extension [T <: DFType](dfVar: DFValNI[T, DFVal.Modifier.Assignable])
    @targetName("workAroundAssign")
    def :=[R](rhs: Exact[R])(using tc: DFVal.TC[T, R], dfc: DFC): Unit =
      dfVar.asInstanceOf[DFVarOf[T]] := rhs
