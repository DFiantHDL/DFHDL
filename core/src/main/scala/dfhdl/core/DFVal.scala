package dfhdl.core
import dfhdl.compiler.ir
import dfhdl.internals.*
import ir.DFVal.Func.Op as FuncOp
import ir.DFVal.Alias.History.Op as HistoryOp
import ir.DFDecimal.NativeType

import scala.annotation.unchecked.uncheckedVariance
import scala.annotation.{implicitNotFound, targetName}
import scala.quoted.*
import DFOpaque.Abstract as DFOpaqueA
import dfhdl.compiler.ir.MemberGetSet
import dfhdl.compiler.printing.{DefaultPrinter, Printer}
import scala.annotation.tailrec

import scala.reflect.ClassTag
final class DFVal[+T <: DFTypeAny, +M <: ModifierAny](val irValue: ir.DFVal | DFError)
    extends AnyVal
    with DFMember[ir.DFVal]
    with Selectable:

  def selectDynamic(name: String)(using DFC): Any = trydf {
    val ir.DFStruct(structName, fieldMap) = this.asIR.dfType: @unchecked
    val dfType = fieldMap(name)
    DFVal.Alias
      .SelectField(this, name)
      .asIR
      .asVal[DFTypeAny, ModifierAny]
  }

  transparent inline def ==[R](
      inline that: R
  )(using DFC): DFValTP[DFBool, Any] = ${
    DFVal.equalityMacro[T, M, R, FuncOp.===.type]('this, 'that)
  }
  transparent inline def !=[R](
      inline that: R
  )(using DFC): DFValTP[DFBool, Any] = ${
    DFVal.equalityMacro[T, M, R, FuncOp.=!=.type]('this, 'that)
  }
end DFVal

type DFValAny = DFVal[DFTypeAny, ModifierAny]
type DFVarAny = DFVal[DFTypeAny, Modifier.Mutable]
type DFDclAny = DFVal[DFTypeAny, Modifier.Dcl]
type DFConstAny = DFVal[DFTypeAny, Modifier.CONST]
type DFValOf[+T <: DFTypeAny] = DFVal[T, ModifierAny]
type DFConstOf[+T <: DFTypeAny] = DFVal[T, Modifier.CONST]
type DFValTP[+T <: DFTypeAny, +P] = DFVal[T, Modifier[Any, Any, Any, P]]
type DFVarOf[+T <: DFTypeAny] = DFVal[T, Modifier.Mutable]

extension (using quotes: Quotes)(tpe: quotes.reflect.TypeRepr)
  def isConstTpe: quotes.reflect.TypeRepr =
    import quotes.reflect.*
    def isConstBool(tpe: TypeRepr): Boolean = tpe.asType match
      case '[DFConstOf[t]] => true
      case '[DFValOf[t]]   => false
      case '[NonEmptyTuple] =>
        tpe.getTupleArgs.forall(isConstBool)
      case '[SameElementsVector[t]] => isConstBool(TypeRepr.of[t])
      case _                        => true
    if (isConstBool(tpe)) TypeRepr.of[CONST]
    else TypeRepr.of[NOTCONST]

extension (using quotes: Quotes)(term: quotes.reflect.Term)
  def checkConst: Boolean =
    import quotes.reflect.*
    extension (term: Term)
      def warn: Boolean =
        report.warning("Not a constant", term.pos)
        false
      def explore: Boolean =
        import quotes.reflect.*
        term match
          case Apply(fun, args)    => fun.explore && args.forall(_.explore)
          case NamedArg(_, expr)   => expr.explore
          case Inlined(_, _, expr) => expr.explore
          case Block(_, expr)      => expr.explore
          case TypeApply(expr, _)  => expr.explore
          case Typed(expr, _)      => expr.explore
          case _ =>
            term.tpe.asType match
              case '[DFConstOf[?]] => true
              case '[DFValOf[?]]   => term.warn
              case _               => true
        end match
      end explore
    end extension
    term.explore
  end checkConst
end extension

infix type <>[T, M] = T match
  case Int => // Int can be a constant literal or just "Int" representing SInt[32]
    IsConst[T] match
      case true  => DFVector.ComposedModifier[T, M]
      case false => DFInt32 <> M
  // This case handles a vector declaration where operator precedence puts an embedded parameter
  // as type for `<>`. E.g.:
  // val len: Int <> CONST = 20
  // val vec1: UInt[4] X len.type <> VAR = all(0)
  // val vec2: UInt[4] X (len.type + 1) <> VAR = all(0)
  // In both cases above the `<>` priority over `X` priority creates a type composition like:
  // `UInt[4] X (len.type <> VAR)`
  // So in this case we construct DFVector.ComposedModifier[T, M] to later used in `X`
  // to properly construct the vector type.
  case DFConstInt32 | IntP.Sig => DFVector.ComposedModifier[T, M]
  case _ =>
    M match
      case DFRET => DFC ?=> DFValOf[DFType.Of[T]]
      case VAL   => DFValOf[DFType.Of[T]]
      case CONST => DFConstOf[DFType.Of[T]]

infix type X[T, M] = M match
  case DFVector.ComposedModifier[d, m] => <>[DFVector[DFType.Of[T], Tuple1[d]], m]
  case _                               => DFVector[DFType.Of[T], Tuple1[M]]
type JUSTVAL[T] = <>[T, VAL]

extension [V <: ir.DFVal](dfVal: V)
  inline def asVal[T <: DFTypeAny, M <: ModifierAny]: DFVal[T, M] =
    DFVal[T, M, V](dfVal)
  inline def asValOf[T <: DFTypeAny]: DFValOf[T] =
    DFVal[T, ModifierAny, V](dfVal)
  inline def asValTP[T <: DFTypeAny, P]: DFValTP[T, P] =
    DFVal[T, Modifier[Any, Any, Any, P], V](dfVal)
  inline def asValAny: DFValAny =
    DFVal[DFTypeAny, ModifierAny, V](dfVal)
  inline def asVarOf[T <: DFTypeAny]: DFVarOf[T] =
    DFVal[T, Modifier.Mutable, V](dfVal)
  inline def asVarAny: DFVarAny =
    DFVal[DFTypeAny, Modifier.Mutable, V](dfVal)
  inline def asDclAny: DFDclAny =
    DFVal[DFTypeAny, Modifier.Dcl, V](dfVal)
  inline def asConstAny[T <: DFTypeAny]: DFConstOf[DFTypeAny] =
    DFVal[DFTypeAny, Modifier.CONST, V](dfVal)
  inline def asConstOf[T <: DFTypeAny]: DFConstOf[T] =
    DFVal[T, Modifier.CONST, V](dfVal)
end extension

extension (dfVal: DFValAny)
  inline def asVal[T <: DFTypeAny, M <: ModifierAny]: DFVal[T, M] =
    dfVal.asInstanceOf[DFVal[T, M]]
  inline def asValOf[T <: DFTypeAny]: DFValOf[T] =
    dfVal.asInstanceOf[DFVal[T, ModifierAny]]
  inline def asValTP[T <: DFTypeAny, P]: DFValTP[T, P] =
    dfVal.asInstanceOf[DFVal[T, Modifier[Any, Any, Any, P]]]
  inline def asVarOf[T <: DFTypeAny]: DFVarOf[T] =
    dfVal.asInstanceOf[DFVal[T, Modifier.Mutable]]
  inline def asVarAny: DFVarAny =
    dfVal.asInstanceOf[DFVal[DFTypeAny, Modifier.Mutable]]
  inline def asDclAny: DFDclAny =
    dfVal.asInstanceOf[DFVal[DFTypeAny, Modifier.Dcl]]
  inline def asConstOf[T <: DFTypeAny]: DFConstOf[T] =
    dfVal.asInstanceOf[DFVal[T, Modifier.CONST]]
end extension

def DFValConversionMacro[T <: DFTypeAny, P, R](
    from: Expr[R]
)(using Quotes, Type[T], Type[P], Type[R]): Expr[DFValTP[T, P]] =
  import quotes.reflect.*
  val fromExactTerm = from.asTerm.exactTerm
  val fromExactType = fromExactTerm.tpe.asTypeOf[Any]
  val fromExactExpr = fromExactTerm.asExpr
  if (TypeRepr.of[P] =:= TypeRepr.of[CONST] && !from.asTerm.checkConst)
    '{ compiletime.error("Applied argument must be a constant.") }
  else
    '{
      import DFStruct.apply
      given bitsNoType: DFBits[Int] = DFNothing.asInstanceOf[DFBits[Int]]
      given uintNoType: DFUInt[Int] = DFNothing.asInstanceOf[DFUInt[Int]]
      given sintNoType: DFSInt[Int] = DFNothing.asInstanceOf[DFSInt[Int]]
      val tc = compiletime.summonInline[DFVal.TC[T, fromExactType.Underlying]]
      val dfc = compiletime.summonInline[DFC]
      val dfType = compiletime.summonInline[T]
      trydf {
        tc(dfType, $fromExactExpr)(using dfc).asValTP[T, P]
      }(using dfc, compiletime.summonInline[CTName])
    }
end DFValConversionMacro

sealed protected trait DFValLP:
  /* TODO: IMPORTANT IMPLICIT CONVERSION ISSUE
     -----------------------------------------
    Currently, the following issue affects the implicit conversion mechanism:
    https://github.com/lampepfl/dotty/issues/19388
    Because of this, we need to explicitly have a solid return type on the conversion definition
    to prevent returning `Nothing`. We have `CONST=:=ISCONST[true]` and `NOTCONST=:=Any`.
    For `NOTCONST`, what actually is happening is that the implicit conversion mechanism
    returns `ISCONST[Boolean]` and therefore we can detect it is not a constant. Because the
    modifier `M` of `DFVal[+T, +M]` is covariant, the compiler accepts
    `DFVal[T, NOTCONST] <:< DFVal[T, ISCONST[Boolean]]` and all is well.
   */
  implicit transparent inline def DFBitsValConversion[
      W <: IntP,
      P <: Boolean,
      R <: DFValAny | SameElementsVector[?] | NonEmptyTuple | Bubble
  ](
      inline from: R
  ): DFValTP[DFBits[W], ISCONST[P]] = ${
    DFValConversionMacro[DFBits[W], ISCONST[P], R]('from)
  }
  // TODO: candidate should be fixed to cause UInt[?]->SInt[Int] conversion
  implicit transparent inline def DFXIntValConversion[
      S <: Boolean,
      W <: IntP,
      N <: NativeType,
      P <: Boolean,
      R <: DFValAny | Int | Bubble
  ](
      inline from: R
  ): DFValTP[DFXInt[S, W, N], ISCONST[P]] = ${
    DFValConversionMacro[DFXInt[S, W, N], ISCONST[P], R]('from)
  }
  implicit transparent inline def DFOpaqueValConversion[
      TFE <: DFOpaque.Abstract,
      P <: Boolean,
      R <: DFValAny | Bubble
  ](
      inline from: R
  ): DFValTP[DFOpaque[TFE], ISCONST[P]] = ${
    DFValConversionMacro[DFOpaque[TFE], ISCONST[P], R]('from)
  }
  implicit transparent inline def DFStructValConversion[
      F <: DFStruct.Fields,
      P <: Boolean,
      R <: DFValAny | DFStruct.Fields | Bubble
  ](
      inline from: R
  ): DFValTP[DFStruct[F], ISCONST[P]] = ${
    DFValConversionMacro[DFStruct[F], ISCONST[P], R]('from)
  }
  implicit transparent inline def DFTupleValConversion[
      T <: NonEmptyTuple,
      P <: Boolean,
      R <: DFValAny | NonEmptyTuple | Bubble
  ](
      inline from: R
  ): DFValTP[DFTuple[T], ISCONST[P]] = ${
    DFValConversionMacro[DFTuple[T], ISCONST[P], R]('from)
  }
  implicit transparent inline def DFVectorValConversion[
      T <: DFTypeAny,
      D <: IntP,
      P <: Boolean,
      R <: DFValAny | Iterable[?] | SameElementsVector[?] | Bubble
  ](
      inline from: R
  ): DFValTP[DFVector[T, Tuple1[D]], ISCONST[P]] = ${
    DFValConversionMacro[DFVector[T, Tuple1[D]], ISCONST[P], R]('from)
  }
  implicit transparent inline def DFBitValConversion[
      P <: Boolean,
      R <: DFValAny | Int | Boolean | Bubble
  ](
      inline from: R
  ): DFValTP[DFBit, ISCONST[P]] = ${
    DFValConversionMacro[DFBit, ISCONST[P], R]('from)
  }
  implicit transparent inline def DFBoolValConversion[
      P <: Boolean,
      R <: DFValAny | Int | Boolean | Bubble
  ](
      inline from: R
  ): DFValTP[DFBool, ISCONST[P]] = ${
    DFValConversionMacro[DFBool, ISCONST[P], R]('from)
  }
  given DFUnitValConversion[R <: DFValAny | Unit | NonEmptyTuple | Bubble](using
      dfc: DFC
  ): Conversion[R, DFValOf[DFUnit]] = from => DFUnitVal().asInstanceOf[DFValOf[DFUnit]]
  given ConstToNonConstAccept[T <: DFTypeAny, P]: Conversion[DFValTP[T, P], DFValTP[T, NOTCONST]] =
    from => from.asValTP[T, NOTCONST]
end DFValLP
object DFVal extends DFValLP:
  // constructing a front-end DFVal value class object. if it's a global value, then
  // we need to save the DFC, instead of the actual member IR object
  inline def apply[T <: DFTypeAny, M <: ModifierAny, IR <: ir.DFVal | DFError](
      irValue: IR
  ): DFVal[T, M] = new DFVal[T, M](irValue)
  inline def unapply(arg: DFValAny): Option[ir.DFVal] = Some(arg.asIR)
  object OrTupleOrStruct:
    def unapply(arg: Any)(using DFC): Option[DFValAny] =
      arg match
        case dfVal: DFValAny     => Some(dfVal)
        case DFTuple.Val(dfVal)  => Some(dfVal)
        case DFStruct.Val(dfVal) => Some(dfVal)
        case _                   => None

  def equalityMacro[T <: DFTypeAny, M <: ModifierAny, R, Op <: FuncOp](
      dfVal: Expr[DFVal[T, M]],
      arg: Expr[R]
  )(using Quotes, Type[T], Type[M], Type[R], Type[Op]): Expr[DFValTP[DFBool, Any]] =
    import quotes.reflect.*
    if (TypeRepr.of[T].typeSymbol equals defn.NothingClass) return '{
      compiletime.error("This is fake")
    }
    val exact = arg.asTerm.exactTerm
    val exactExpr = exact.asExpr
    val exactType = exact.tpe.asTypeOf[Any]
    val lpType = dfVal.asTerm.tpe.isConstTpe.asTypeOf[Any]
    val rpType = exact.tpe.isConstTpe.asTypeOf[Any]
    '{
      val c = compiletime.summonInline[
        DFVal.Compare[T, exactType.Underlying, Op, false]
      ]
      val dfc = compiletime.summonInline[DFC]
      c($dfVal, $exactExpr)(using
        dfc,
        compiletime.summonInline[ValueOf[Op]],
        new ValueOf[false](false)
      )
        // TODO: May not be need if this issue is solved:
        // https://github.com/lampepfl/dotty/issues/19554
        .asValTP[DFBool, lpType.Underlying | rpType.Underlying]
    }
  end equalityMacro

  // Enabling equality with Int, Boolean, and Tuples.
  // just to give a better error message via the compiler plugin.
  // See the method `rejectBadPrimitiveOps` in `MetaContextGenPhase.scala`
  given [T <: DFTypeAny, M <: ModifierAny]: CanEqual[Int, DFVal[T, M]] =
    CanEqual.derived
  given [T <: DFTypeAny, M <: ModifierAny]: CanEqual[Boolean, DFVal[T, M]] =
    CanEqual.derived
  given [T <: DFTypeAny, M <: ModifierAny]: CanEqual[Tuple, DFVal[T, M]] =
    CanEqual.derived
  // Enabling encoding comparison
  given [T <: DFTypeAny, M <: ModifierAny]: CanEqual[DFEncoding, DFVal[T, M]] =
    CanEqual.derived

  given __refined_dfVal[T <: FieldsOrTuple, A, I, P](using
      r: DFStruct.Val.Refiner[T, A, I, P]
  ): Conversion[DFVal[DFStruct[T], Modifier[A, Any, I, P]], r.Out] = _.asInstanceOf[r.Out]

  trait ConstCheck[P]
  given [P](using
      AssertGiven[
        P =:= CONST,
        "Only a DFHDL constant is convertible to a Scala value, but this DFHDL value is not a constant."
      ]
  ): ConstCheck[P] with {}

  extension [D, T <: ir.DFType, P](lhs: DFValTP[DFType[ir.DFType.Aux[T, Option[D]], ?], P])
    protected[core] def toScalaValue(using dfc: DFC, check: ConstCheck[P]): D =
      import dfc.getSet
      val lhsIR = lhs.asIR
      def error(errMsg: String): Nothing =
        exitWithError(
          s"""|Scala value access error!
              |Position:  ${lhsIR.meta.position}
              |Hierarchy: ${lhsIR.getOwnerDesign.getFullName}
              |Message:   ${errMsg}""".stripMargin
        )
      lhsIR.getParamData.asInstanceOf[Option[Option[D]]]
        .getOrElse(error("Cannot fetch a Scala value from a non-constant DFHDL value."))
        .getOrElse(error("Cannot fetch a Scala value from a bubble (invalid) DFHDL value."))

  trait InitCheck[I]
  given [I](using
      initializableOnly: AssertGiven[
        I =:= Modifier.Initializable,
        "Can only initialize a DFHDL port or variable that are not already initialized."
      ]
  ): InitCheck[I] with {}

  extension [T <: DFTypeAny, M <: ModifierAny](dfVal: DFVal[T, M])
    @metaContextForward(0)
    infix def tag[CT <: ir.DFTag: ClassTag](customTag: CT)(using
        dfc: DFC
    ): DFVal[T, M] =
      import dfc.getSet
      dfVal.asIR
        .setTags(_.tag(customTag))
        .setMeta(m => if (m.isAnonymous && !dfc.getMeta.isAnonymous) dfc.getMeta else m)
        .asVal[T, M]
    @metaContextForward(0)
    infix def tag[CT <: ir.DFTag: ClassTag](condCustomTag: Conditional[CT])(using
        dfc: DFC
    ): DFVal[T, M] = if (condCustomTag.isActive) dfVal.tag(condCustomTag.getArg) else dfVal
    def hasTag[CT <: ir.DFTag: ClassTag](using dfc: DFC): Boolean =
      import dfc.getSet
      dfVal.asIR.tags.hasTagOf[CT]
    @metaContextForward(0)
    infix def setName(name: String)(using dfc: DFC): DFVal[T, M] =
      import dfc.getSet
      dfVal.asIR
        .setMeta(m =>
          if (m.isAnonymous && !dfc.getMeta.isAnonymous) dfc.getMeta.setName(name)
          else m.setName(name)
        )
        .asVal[T, M]
    def anonymize(using dfc: DFC): DFVal[T, M] =
      import dfc.getSet
      dfVal.asIR match
        case dfValIR: (ir.DFVal.Alias | ir.DFVal.Const | ir.DFVal.Func) =>
          dfValIR.setMeta(m => m.anonymize).asVal[T, M]
        case _ => dfVal
    def inDFCPosition(using DFC): Boolean = dfVal.asIR.meta.position == dfc.getMeta.position
    def anonymizeInDFCPosition(using DFC): DFVal[T, M] =
      if (inDFCPosition) dfVal.anonymize else dfVal
    @metaContextForward(0)
    def nameInDFCPosition(using dfc: DFC): DFVal[T, M] =
      import dfc.getSet
      val dfValIR = dfVal.asIR
      if (inDFCPosition && dfValIR.isAnonymous && !dfc.isAnonymous)
        dfValIR.setMeta(_ => dfc.getMeta).asVal[T, M]
      else dfVal
  end extension

  case object ExtendTag extends ir.DFTagOf[ir.DFVal]
  type ExtendTag = ExtendTag.type
  case object TruncateTag extends ir.DFTagOf[ir.DFVal]
  type TruncateTag = TruncateTag.type

  @metaContextForward(0)
  trait InitValue[T <: DFTypeAny]:
    def enable: Boolean
    def apply(dfType: T)(using dfc: DFC): DFConstOf[T]
  object InitValue:
    transparent inline implicit def fromValue[T <: DFTypeAny, V](
        inline value: V
    ): InitValue[T] = ${ fromValueMacro[T, V]('value) }

    def fromValueMacro[T <: DFTypeAny, V](
        value: Expr[V]
    )(using Quotes, Type[T], Type[V]): Expr[InitValue[T]] =
      import quotes.reflect.*
      val (argExpr, enableExpr) = value match
        case '{ Conditional.Ops.@@[t]($x)($y) } => (x, y)
        case _                                  => (value, '{ true })
      if (argExpr.asTerm.checkConst)
        val term = argExpr.asTerm.exactTerm
        val tpe = term.tpe.asTypeOf[Any]
        '{
          val tc = compiletime.summonInline[DFVal.TC[T, tpe.Underlying]]
          new InitValue[T]:
            def enable: Boolean = $enableExpr
            def apply(dfType: T)(using dfc: DFC): DFConstOf[T] =
              tc(dfType, ${ term.asExpr })(using dfc).asConstOf[T]
        }
      else '{ compiletime.error("Init value must be a constant.") }
    end fromValueMacro
  end InitValue

  @metaContextForward(0)
  trait InitTupleValues[T <: NonEmptyTuple]:
    def enable: Boolean
    def apply(dfType: DFTuple[T])(using dfc: DFC): List[DFConstOf[DFTuple[T]]]
  object InitTupleValues:
    transparent inline implicit def fromValue[T <: NonEmptyTuple, V](
        inline value: V
    ): InitTupleValues[T] = ${ fromValueMacro[T, V]('value) }

    def fromValueMacro[T <: NonEmptyTuple, V](
        value: Expr[V]
    )(using Quotes, Type[T], Type[V]): Expr[InitTupleValues[T]] =
      import quotes.reflect.*
      val (argExpr, enableExpr) = value match
        case '{ Conditional.Ops.@@[t]($x)($y) } => (x, y)
        case _                                  => (value, '{ true })
      val term = argExpr.asTerm.underlyingArgument
      if (term.checkConst)
        val tTpe = TypeRepr.of[T]
        extension (lhs: TypeRepr)
          def tupleSigMatch(
              rhs: TypeRepr,
              tupleAndNonTupleMatch: Boolean
          ): Boolean =
            import quotes.reflect.*
            (lhs.asType, rhs.asType) match
              case ('[DFTuple[t]], '[Any]) =>
                TypeRepr.of[t].tupleSigMatch(rhs, tupleAndNonTupleMatch)
              case ('[Tuple], '[Tuple]) =>
                val lArgs = lhs.getTupleArgs
                val rArgs = rhs.getTupleArgs
                if (lArgs.length != rArgs.length) false
                else
                  (lArgs lazyZip rArgs).forall((l, r) => l.tupleSigMatch(r, true))
              case ('[Tuple], '[Any]) => tupleAndNonTupleMatch
              case ('[Any], '[Tuple]) => tupleAndNonTupleMatch
              case _                  => true
          end tupleSigMatch
        end extension

        val vTpe = term.tpe
        val multiElements = vTpe.asTypeOf[Any] match
          case '[NonEmptyTuple] =>
            vTpe.getTupleArgs.forall(va => tTpe.tupleSigMatch(va, false))
          case _ => false
        // In the case we have a multiple elements in the tuple value that match the signature
        // of the DFHDL type, then each element is considered as a candidate
        if (multiElements)
          val Apply(_, vArgsTerm) = term: @unchecked
          def inits(dfType: Expr[DFTuple[T]], dfc: Expr[DFC]): List[Expr[DFConstOf[DFTuple[T]]]] =
            vArgsTerm.map { a =>
              val aTerm = a.exactTerm
              val aType = aTerm.tpe.asTypeOf[Any]
              '{
                val tc = compiletime.summonInline[DFVal.TC[DFTuple[T], aType.Underlying]]
                tc($dfType, ${ aTerm.asExpr })(using $dfc).asConstOf[DFTuple[T]]
              }
            }
          '{
            new InitTupleValues[T]:
              def enable: Boolean = $enableExpr
              def apply(dfType: DFTuple[T])(using dfc: DFC): List[DFConstOf[DFTuple[T]]] =
                List(${ Expr.ofList(inits('dfType, 'dfc)) }*)
          }
        // otherwise the entire tuple is considered as a candidate.
        else
          val vTerm = term.exactTerm
          val vType = vTerm.tpe.asTypeOf[Any]
          '{
            val tc = compiletime.summonInline[DFVal.TC[DFTuple[T], vType.Underlying]]
            new InitTupleValues[T]:
              def enable: Boolean = $enableExpr
              def apply(dfType: DFTuple[T])(using dfc: DFC): List[DFConstOf[DFTuple[T]]] =
                List(tc(dfType, ${ vTerm.asExpr })(using dfc).asConstOf[DFTuple[T]])
          }
        end if
      else '{ compiletime.error("Init value must be a constant.") }
      end if
    end fromValueMacro
  end InitTupleValues

  extension [T <: DFTypeAny, A, C, I, P, R](dfVal: DFVal[T, Modifier[A, C, I, P]])
    private[core] def initForced(initValues: List[DFConstOf[T]])(using
        dfc: DFC
    ): DFVal[T, Modifier[A, C, Modifier.Initialized, P]] =
      import dfc.getSet
      require(
        dfVal.asIR.isAnonymous,
        s"Cannot initialize a named value ${dfVal.asIR.getFullName}. Initialization is only supported at the declaration of the value."
      )
      val modifier = Modifier[A, C, I, P](dfVal.asIR.asInstanceOf[ir.DFVal.Dcl].modifier)
      // We do not need to replace the original Dcl, because it was anonymous and not added to the
      // mutable DB. Also see comment in `DFVal.Dcl`.
      DFVal.Dcl(dfVal.dfType, modifier, initValues)
        .asVal[T, Modifier[A, C, Modifier.Initialized, P]]
    end initForced

    infix def init(
        initValues: InitValue[T]*
    )(using DFC, InitCheck[I]): DFVal[T, Modifier[A, C, Modifier.Initialized, P]] = trydf {
      val tvList =
        initValues.view.filter(_.enable).map(tv => tv(dfVal.dfType)(using dfc.anonymize)).toList
      dfVal.initForced(tvList)
    }
  end extension
  extension [T <: NonEmptyTuple, A, C, I, P](dfVal: DFVal[DFTuple[T], Modifier[A, C, I, P]])
    infix def init(
        initValues: InitTupleValues[T]
    )(using DFC, InitCheck[I]): DFVal[DFTuple[T], Modifier[A, C, Modifier.Initialized, P]] =
      trydf {
        if (initValues.enable)
          dfVal.initForced(initValues(dfVal.dfType)(using dfc.anonymize))
        else dfVal.initForced(Nil)
      }

  implicit def BooleanHack(from: DFValOf[DFBoolOrBit])(using DFC): Boolean =
    ???

  // opaque values need special conversion that does not try to summon the opaque dftype
  // because it can be abstract in extension methods that are applied generically on an abstract
  // opaque super-type. E.g.:
  // ```
  // abstract class MyAbsOpaque extends Opaque
  // case class MyOpaque extends MyAbsOpaque
  // extension (a : MyAbsOpaque <> VAL) def foo : Unit = {}
  // val a = MyOpaque <> VAR
  // a.foo //here we currently access `foo` through conversion to MyAbsOpaque
  //       //because DFOpaque is not completely covariant due to bug
  //       //https://github.com/lampepfl/dotty/issues/15704
  // ```
  given DFOpaqueValConversion[T <: DFOpaque.Abstract, R <: DFOpaque.Abstract](using
      DFC,
      R <:< T
  ): Conversion[DFValOf[DFOpaque[R]], DFValOf[DFOpaque[T]]] = from =>
    from.asInstanceOf[DFValOf[DFOpaque[T]]]

  object Const:
    def apply[IRT <: ir.DFType, D, T <: DFType[ir.DFType.Aux[IRT, D], ?]](
        dfType: T,
        data: D,
        named: Boolean = false
    )(using
        DFC
    ): DFConstOf[T] = forced(dfType, data, named)
    def forced[T <: DFTypeAny](
        dfType: T,
        data: Any,
        named: Boolean = false
    )(using DFC): DFConstOf[T] =
      val meta = if (named) dfc.getMeta else dfc.getMeta.anonymize
      ir.DFVal
        .Const(dfType.asIR, data, dfc.ownerOrEmptyRef, meta, ir.DFTags.empty)
        .addMember
        .asConstOf[T]
  end Const

  object Open:
    def apply[T <: DFTypeAny](dfType: T)(using DFC): DFValOf[T] =
      ir.DFVal.Open(dfType.asIR, dfc.owner.ref)
        .addMember
        .asValOf[T]

  object Dcl:
    def apply[T <: DFTypeAny, M <: ModifierAny](
        dfType: T,
        modifier: M,
        initValues: List[DFConstOf[T]] = Nil
    )(using
        DFC
    ): DFVal[T, M] =
      // Anonymous Dcls are only supposed to be followed by an `init` that will construct the
      // fully initialized Dcl. The reason for this behavior is that we do not want to add the
      // Dcl to the mutable DB and only after add its initialization value to the DB, thereby
      // violating the reference order rule (we can only reference values that appear before).
      if (dfc.isAnonymous && initValues.isEmpty)
        ir.DFVal.Dcl(
          dfType.asIR,
          modifier.asIR,
          Nil,
          ir.DFRef.OneWay.Empty,
          dfc.getMeta,
          ir.DFTags.empty
        ).asVal[T, M]
      else
        val dcl: ir.DFVal.Dcl = ir.DFVal.Dcl(
          dfType.asIR,
          modifier.asIR,
          initValues.map(_.asIR.refTW[ir.DFVal.Dcl]),
          dfc.owner.ref,
          dfc.getMeta,
          ir.DFTags.empty
        )
        dcl.addMember.asVal[T, M]
    end apply
  end Dcl

  object Func:
    export ir.DFVal.Func.Op
    def apply[T <: DFTypeAny, P](
        dfType: T,
        op: FuncOp,
        args: List[DFValTP[?, P]]
    )(using DFC): DFValTP[T, P] =
      args.foreach(_.anonymizeInDFCPosition)
      apply(dfType, op, args.map(_.asIR))
    @targetName("applyFromIR")
    def apply[T <: DFTypeAny, P](
        dfType: T,
        op: FuncOp,
        args: List[ir.DFVal]
    )(using DFC): DFValTP[T, P] =
      val func: ir.DFVal = ir.DFVal.Func(
        dfType.asIR,
        op,
        args.map(_.refTW[ir.DFVal]),
        dfc.ownerOrEmptyRef,
        dfc.getMeta,
        ir.DFTags.empty
      )
      func.addMember.asValTP[T, P]
    end apply
  end Func

  object Alias:
    object AsIs:
      def apply[AT <: DFTypeAny, VT <: DFTypeAny, M <: ModifierAny](
          aliasType: AT,
          relVal: DFVal[VT, M],
          forceNewAlias: Boolean = false
      )(using dfc: DFC): DFVal[AT, M] =
        relVal.asIR match
          // anonymous constant are replaced by a different constant
          // after its data value was converted according to the alias
          case const: ir.DFVal.Const
              if (const.isAnonymous || relVal.inDFCPosition) && !forceNewAlias =>
            val updatedData = ir.dataConversion(aliasType.asIR, const.dfType)(
              const.data.asInstanceOf[const.dfType.Data]
            )(using dfc.getSet)
            dfc.mutableDB.setMember(
              const,
              _.copy(dfType = aliasType.asIR, data = updatedData, meta = dfc.getMeta)
            ).asVal[AT, M]
          // named constants or other non-constant values are referenced
          // in a new alias construct
          case _ =>
            forced(aliasType.asIR, relVal.anonymizeInDFCPosition.asIR).asVal[AT, M]
      end apply
      def forced(aliasType: ir.DFType, relVal: ir.DFVal)(using DFC): ir.DFVal =
        val alias: ir.DFVal.Alias.AsIs =
          ir.DFVal.Alias.AsIs(
            aliasType,
            relVal.refTW[ir.DFVal.Alias.AsIs],
            dfc.ownerOrEmptyRef,
            dfc.getMeta,
            ir.DFTags.empty
          )
        alias.addMember
      def ident[T <: DFTypeAny, M <: ModifierAny](relVal: DFVal[T, M])(using
          DFC
      ): DFVal[T, M] =
        import ir.DFVal.Alias.IdentTag
        apply(relVal.dfType, relVal, forceNewAlias = true).tag(IdentTag)
      def bind[T <: DFTypeAny, M <: ModifierAny](relVal: DFVal[T, M], bindName: String)(using
          DFC
      ): DFVal[T, M] =
        import ir.DFConditional.DFCaseBlock.Pattern
        ident(relVal)(using dfc.setName(bindName)).tag(Pattern.Bind.Tag)
    end AsIs
    object History:
      def apply[T <: DFTypeAny](
          relVal: DFValOf[T],
          step: Int,
          op: HistoryOp,
          initOption: Option[DFConstOf[T]]
      )(using DFC): DFValOf[T] =
        val alias: ir.DFVal.Alias.History =
          ir.DFVal.Alias.History(
            relVal.dfType.asIR,
            relVal.asIR.refTW[ir.DFVal.Alias.History],
            step,
            op,
            initOption.map(_.asIR.refTW[ir.DFVal.Alias.History]),
            dfc.owner.ref,
            dfc.getMeta,
            ir.DFTags.empty
          )
        alias.addMember.asValOf[T]
      end apply
    end History
    object ApplyRange:
      def apply[W <: IntP, M <: ModifierAny, H <: Int, L <: Int](
          relVal: DFVal[DFBits[W], M],
          relBitHigh: Inlined[H],
          relBitLow: Inlined[L]
      )(using DFC): DFVal[DFBits[H - L + 1], M] =
        forced(relVal.asIR, relBitHigh, relBitLow).asVal[DFBits[H - L + 1], M]
      end apply
      def forced(
          relVal: ir.DFVal,
          relBitHigh: Int,
          relBitLow: Int
      )(using DFC): ir.DFVal =
        relVal match
          // anonymous constant are replace by a different constant
          // after its data value was converted according to the alias
          case const: ir.DFVal.Const if const.isAnonymous =>
            val updatedData =
              ir.selBitRangeData(
                const.data.asInstanceOf[(BitVector, BitVector)],
                relBitHigh,
                relBitLow
              )
            Const.forced(DFBits(relBitHigh - relBitLow + 1), updatedData).asIR
          // named constants or other non-constant values are referenced
          // in a new alias construct
          case _ =>
            val alias: ir.DFVal.Alias.ApplyRange =
              ir.DFVal.Alias.ApplyRange(
                relVal.refTW[ir.DFVal.Alias.ApplyRange],
                relBitHigh,
                relBitLow,
                dfc.ownerOrEmptyRef,
                dfc.getMeta,
                ir.DFTags.empty
              )
            alias.addMember
      end forced
    end ApplyRange
    object ApplyIdx:
      def apply[
          T <: DFTypeAny,
          W <: IntP,
          M <: ModifierAny,
          IS <: Boolean,
          IW <: Int,
          IN <: NativeType
      ](
          dfType: T,
          relVal: DFVal[DFTypeAny, M],
          relIdx: DFValOf[DFXInt[IS, IW, IN]]
      )(using DFC): DFVal[T, M] =
        val alias: ir.DFVal.Alias.ApplyIdx =
          ir.DFVal.Alias.ApplyIdx(
            dfType.asIR,
            relVal.asIR.refTW[ir.DFVal.Alias.ApplyIdx],
            relIdx.asIR.refTW[ir.DFVal.Alias.ApplyIdx],
            dfc.ownerOrEmptyRef,
            dfc.getMeta,
            ir.DFTags.empty
          )
        alias.addMember.asVal[T, M]
      end apply
    end ApplyIdx
    object SelectField:
      def apply[T <: DFTypeAny, M <: ModifierAny](
          relVal: DFVal[DFTypeAny, M],
          fieldName: String
      )(using dfc: DFC): DFVal[T, M] =
        val relValIR = relVal.asIR
        val ir.DFStruct(_, fieldMap) = relValIR.dfType: @unchecked
        val dfTypeIR = fieldMap(fieldName)
        relValIR match
          // in case the referenced value is anonymous and concatenates fields
          // of values, then we just directly reference the relevant
          // value.
          case ir.DFVal.Func(_, FuncOp.++, args, _, meta, _) if meta.isAnonymous =>
            import dfc.getSet
            val idx = fieldMap.keys.toList.indexWhere(_ == fieldName)
            args(idx).get.asVal[T, M]
          // for all other case create a selector
          case _ =>
            val alias: ir.DFVal.Alias.SelectField =
              ir.DFVal.Alias.SelectField(
                dfTypeIR,
                relValIR.refTW[ir.DFVal.Alias.SelectField],
                fieldName,
                dfc.owner.ref,
                dfc.getMeta,
                ir.DFTags.empty
              )
            alias.addMember.asVal[T, M]
        end match
      end apply
    end SelectField
  end Alias

  trait TC[T <: DFTypeAny, R] extends TCConv[T, R, DFValAny]:
    type OutP
    type Out = DFValTP[T, OutP]
    final def apply(dfType: T, value: R)(using DFC): Out = trydf:
      conv(dfType, value)

  trait TCLP:
    // Accept OPEN in compile-time, but throw exception where it should not be used
    given fromOPEN[T <: DFTypeAny]: TC[T, r__OPEN.type] with
      def conv(dfType: T, value: r__OPEN.type)(using DFC): Out =
        throw new IllegalArgumentException("OPEN cannot be used here")
    // Accept any bubble value
    given fromBubble[T <: DFTypeAny, V <: Bubble]: TC[T, V] with
      type OutP = CONST
      def conv(dfType: T, value: V)(using DFC): Out = Bubble.constValOf(dfType, named = true)
    transparent inline given errorDMZ[T <: DFTypeAny, R](using
        t: ShowType[T],
        r: ShowType[R]
    ): TC[T, R] =
      Error.call[
        (
            "Unsupported value of type `",
            r.Out,
            "` for DFHDL receiver type `",
            t.Out,
            "`."
        )
      ]
    given sameValType[T <: DFTypeAny, P, V <: DFValTP[T, P]]: TC[T, V] with
      type OutP = P
      def conv(dfType: T, value: V)(using dfc: DFC): DFValTP[T, P] =
        import dfc.getSet
        given Printer = DefaultPrinter
        val ret: DFValAny =
          if (dfType != value.dfType)
            (dfType.asIR, value.dfType.asIR) match
              case (_: ir.DFBits, _: ir.DFBits) =>
                DFBits.Val.TC(dfType.asFE[DFBits[Int]], value.asValOf[DFBits[Int]])
              case (ir.DFXInt(_, _, _), ir.DFXInt(_, _, _)) =>
                DFXInt.Val.TC(
                  dfType.asFE[DFXInt[Boolean, Int, NativeType]],
                  value.asValOf[DFXInt[Boolean, Int, NativeType]]
                )
              case _ =>
                throw new IllegalArgumentException(
                  s"Unsupported value of type `${value.dfType.codeString}` for DFHDL receiver type `${dfType.codeString}`."
                )
          else value
        ret.asValTP[T, P]
      end conv
    end sameValType
  end TCLP
  object TC extends TCLP:
    export DFBoolOrBit.Val.TC.given
    export DFBits.Val.TC.given
    export DFDecimal.Val.TC.given
    export DFEnum.Val.TC.given
    export DFVector.Val.TC.given
    export DFTuple.Val.TC.given
    export DFStruct.Val.TC.given
    export DFOpaque.Val.TC.given
  end TC

  trait Compare[T <: DFTypeAny, V, Op <: FuncOp, C <: Boolean] extends TCConv[T, V, DFValAny]:
    type OutP
    type Out = DFValTP[T, OutP]
    final protected def func[P1, P2](arg1: DFValTP[?, P1], arg2: DFValTP[?, P2])(using
        DFC,
        ValueOf[Op],
        ValueOf[C]
    ): DFValTP[DFBool, P1 | P2] =
      val list = if (valueOf[C]) List(arg2, arg1) else List(arg1, arg2)
      DFVal.Func(DFBool, valueOf[Op], list)
    def apply[P](dfVal: DFValTP[T, P], arg: V)(using
        DFC,
        ValueOf[Op],
        ValueOf[C]
    ): DFValTP[DFBool, P | OutP] = trydf:
      val dfValArg = conv(dfVal.dfType, arg)(using dfc.anonymize)
      func(dfVal, dfValArg)
  end Compare
  trait CompareLP:
    transparent inline given errorDMZ[
        T <: DFTypeAny,
        R,
        Op <: FuncOp,
        C <: Boolean
    ](using
        t: ShowType[T],
        r: ShowType[R]
    ): Compare[T, R, Op, C] =
      Error.call[
        (
            "Cannot compare DFHDL value of type `",
            t.Out,
            "` with value of type `",
            r.Out,
            "`."
        )
      ]
    given sameValType[T <: DFTypeAny, P, R <: DFValTP[T, P], Op <: FuncOp, C <: Boolean](using
        ValueOf[Op],
        ValueOf[C]
    ): Compare[T, R, Op, C] with
      type OutP = P
      def conv(dfType: T, arg: R)(using dfc: DFC): Out =
        import dfc.getSet
        given Printer = DefaultPrinter
        require(
          dfType == arg.dfType,
          s"Cannot compare DFHDL value type `${dfType.codeString}` with DFHDL value type `${arg.dfType.codeString}`."
        )
        arg
  end CompareLP
  object Compare extends CompareLP:
    export DFBoolOrBit.Val.Compare.given
    export DFBits.Val.Compare.given
    export DFDecimal.Val.Compare.given
    export DFEnum.Val.Compare.given
    export DFVector.Val.Compare.given
    export DFTuple.Val.Compare.given
    export DFStruct.Val.Compare.given
  end Compare

  trait DFDomainOnly
  given (using domain: DomainType)(using
      AssertGiven[
        domain.type <:< DomainType.DF,
        "This construct is only available in a dataflow domain."
      ]
  ): DFDomainOnly with {}
  trait RTDomainOnly
  given (using domain: DomainType)(using
      AssertGiven[
        domain.type <:< DomainType.RT,
        "This construct is only available in a register-transfer domain."
      ]
  ): RTDomainOnly with {}
  trait PrevInitCheck[I]
  given [I](using
      AssertGiven[
        I =:= Modifier.Initialized,
        "Value must be an initialized declaration or `.prev` must have an initialization argument.\nE.g.: `x.prev(step, init)`.\nIt's possible to apply a bubble initialization with `init = ?`"
      ]
  ): PrevInitCheck[I] with {}
  trait RegInitCheck[I]
  given [I](using
      AssertGiven[
        I =:= Modifier.Initialized,
        "Value must be an initialized declaration or `.reg` must have an initialization argument.\nE.g.: `x.reg(step, init)`.\nIt's possible to apply an unknown initialization with `init = ?`"
      ]
  ): RegInitCheck[I] with {}
  // exporting apply here to reduce common case of possible ambiguities when additional apply methods are defined
  export DFBits.Val.Ops.apply
  export DFVector.Val.Ops.apply
  export DFTuple.Val.Ops.apply
  object Ops:
    extension [T <: DFTypeAny, A, C, I, S <: Int, V](dfVal: DFVal[T, Modifier[A, C, I, Any]])
      def prev(step: Inlined[S], init: InitValue[T])(using
          dfc: DFC,
          dfOnly: DFDomainOnly,
          check: Arg.Positive.Check[S]
      ): DFValOf[T] = trydf {
        check(step)
        val initOpt = Some(init(dfVal.dfType)(using dfc.anonymize))
        DFVal.Alias.History(dfVal, step, HistoryOp.State, initOpt)
      }
      def prev(step: Inlined[S])(using
          dfc: DFC,
          dfOnly: DFDomainOnly,
          initCheck: PrevInitCheck[I],
          check: Arg.Positive.Check[S]
      ): DFValOf[T] = trydf {
        check(step)
        DFVal.Alias.History(dfVal, step, HistoryOp.State, None)
      }
      inline def prev(using DFDomainOnly, PrevInitCheck[I], DFC): DFValOf[T] = dfVal.prev(1)
      def pipe(
          step: Inlined[S]
      )(using dfOnly: DFDomainOnly, dfc: DFC, check: Arg.Positive.Check[S]): DFValOf[T] = trydf {
        check(step)
        DFVal.Alias.History(
          dfVal,
          step,
          HistoryOp.Pipe,
          // pipe always has a bubble for initialization
          Some(Bubble.constValOf(dfVal.dfType, named = false))
        )
      }
      inline def pipe(using DFC, DFDomainOnly): DFValOf[T] = dfVal.pipe(1)
      def reg(step: Inlined[S])(using
          dfc: DFC,
          rtOnly: RTDomainOnly,
          initCheck: RegInitCheck[I],
          check: Arg.Positive.Check[S]
      ): DFValOf[T] = trydf {
        check(step)
        DFVal.Alias.History(dfVal, step, HistoryOp.State, None)
      }
      def reg(step: Inlined[S], init: InitValue[T])(using
          dfc: DFC,
          rtOnly: RTDomainOnly,
          check: Arg.Positive.Check[S]
      ): DFValOf[T] = trydf {
        check(step)
        val initOpt = Some(init(dfVal.dfType)(using dfc.anonymize))
        DFVal.Alias.History(dfVal, step, HistoryOp.State, initOpt)
      }
      inline def reg(using DFC, RTDomainOnly, RegInitCheck[I]): DFValOf[T] = dfVal.reg(1)
      def width(using DFC): DFConstInt32 = dfVal.widthIntParam.toDFConst
      def widthInt(using DFC): Int = dfVal.widthIntParam.toScalaInt
    end extension

    extension [T <: DFTypeAny, A, C, I, P](dfVal: DFVal[T, Modifier[A, C, I, P]])
      def bits(using w: Width[T])(using DFC): DFValTP[DFBits[w.Out], P] = trydf {
        DFVal.Alias.AsIs(DFBits(dfVal.widthIntParam), dfVal)
      }
      def genNewVar(using DFC): DFVarOf[T] = trydf {
        val meta = dfVal.asIR.meta.copy(nameOpt = dfc.nameOpt, docOpt = None)
        DFVal.Dcl(dfVal.dfType, Modifier.VAR)(using dfc.setMeta(meta))
      }
    end extension
  end Ops
end DFVal

extension [T <: DFTypeAny](dfVar: DFValOf[T])
  def assign[R <: DFTypeAny](rhs: DFValOf[R])(using DFC): Unit =
    DFNet(dfVar.asIR, DFNet.Op.Assignment, rhs.asIR)
  def nbassign[R <: DFTypeAny](rhs: DFValOf[R])(using DFC): Unit =
    DFNet(dfVar.asIR, DFNet.Op.NBAssignment, rhs.asIR)

extension [T <: DFTypeAny](lhs: DFValOf[T])
  def connect[R <: DFTypeAny](rhs: DFValOf[R])(using DFC): Unit =
    val op = if (dfc.lateConstruction) DFNet.Op.ViaConnection else DFNet.Op.Connection
    DFNet(lhs.asIR, op, rhs.asIR)

trait VarsTuple[T <: NonEmptyTuple]:
  type Width <: Int
object VarsTuple:
  transparent inline given [T <: NonEmptyTuple]: VarsTuple[T] = ${ evMacro[T] }
  def evMacro[T <: NonEmptyTuple](using Quotes, Type[T]): Expr[VarsTuple[T]] =
    import quotes.reflect.*
    val tTpe = TypeRepr.of[T]
    def varsCheck(tpe: TypeRepr): Option[String] =
      tpe.asTypeOf[Any] match
        case '[DFVarOf[t]] => None
        case '[NonEmptyTuple] =>
          tpe.getTupleArgs.view.map(varsCheck).collectFirst { case Some(v) => v }
        case _ =>
          println(tpe.widen.dealias.show)
          Some(s"All tuple elements must be mutable but found an immutable type `${tpe.showType}`")
    varsCheck(tTpe) match
      case Some(err) => '{ compiletime.error(${ Expr(err) }) }
      case None =>
        import Width.calcValWidth
        val widthType = tTpe.calcValWidth.asTypeOf[Int]
        '{
          new VarsTuple[T]:
            type Width = widthType.Underlying
        }
  end evMacro
end VarsTuple

final class REG_DIN[T <: DFTypeAny](val irValue: DFError.REG_DIN[T]) extends AnyVal:
  def :=[R](rhs: Exact[R])(using
      tc: DFVal.TC[T, R],
      dfc: DFC
  ): Unit = trydf {
    val dfVar = irValue.dfVar
    dfVar.assign(tc(dfVar.dfType, rhs))
  }

object DFVarOps:
  protected type NotREG[A] = AssertGiven[
    util.NotGiven[A <:< Modifier.AssignableREG],
    "Cannot assign to a register output; it is immutable.\nTo assign to the register's input, apply `.din` on the LHS argument of the assignment."
  ]
  protected type IsREG[A] = AssertGiven[
    A <:< Modifier.AssignableREG,
    "This is not an assignable register."
  ]
  protected type VarOnly[A] = AssertGiven[
    A <:< Modifier.Assignable,
    "Cannot assign to an immutable value."
  ]
  protected type LocalOrNonED[A] = AssertGiven[
    (A <:< DFC.Scope.Process) | util.NotGiven[A <:< DomainType.ED],
    "Blocking assignment `:=` is not allowed for a non-local variable in this domain.\nChange the assignment to a non-blocking assignment `:==` or the position of the defined variable."
  ]
  protected type NotLocalVar[A] = AssertGiven[
    util.NotGiven[A <:< DFC.Scope.Process],
    "Non-blocking assignment `:==` is not allowed for a local variable (defined inside the process block).\nChange the assignment to a blocking assignment `:=` or the position of the defined variable."
  ]
  protected type EDDomainOnly[A] = AssertGiven[
    A <:< DomainType.ED,
    "Non-blocking assignment `:==` is allowed only inside an event-driven (ED) domain.\nChange the assignment to a regular assignment `:=` or the logic domain to ED."
  ]
  protected type InsideProcess[A] = AssertGiven[
    DFC.Scope.Process | util.NotGiven[A <:< DomainType.ED],
    "Assignments `:=`/`:==` are only allowed inside a process under an event-driven (ED) domain.\nChange the assignment to a connection `<>` or place it in a process."
  ]
  protected type RTDomainOnly[A] = AssertGiven[
    A <:< DomainType.RT,
    "`.din` selection is only allowed under register-transfer (RT) domains."
  ]
  extension [T <: DFTypeAny, A](dfVar: DFVal[T, Modifier[A, Any, Any, Any]])
    def :=[R](rhs: Exact[R])(using
        notREG: NotREG[A],
        varOnly: VarOnly[A],
//        localOrNonED: LocalOrNonED[A],
        insideProcess: InsideProcess[A],
        tc: DFVal.TC[T, R],
        dfc: DFC
    ): Unit = trydf {
      dfVar.assign(tc(dfVar.dfType, rhs))
    }
    def :==[R](rhs: Exact[R])(using dt: DomainType)(using
        varOnly: VarOnly[A],
        edDomainOnly: EDDomainOnly[dt.type],
//        notLocalVar: NotLocalVar[A],
        insideProcess: InsideProcess[A],
        tc: DFVal.TC[T, R],
        dfc: DFC
    ): Unit = trydf {
      dfVar.nbassign(tc(dfVar.dfType, rhs))
    }
    def din(using dt: DomainType)(using IsREG[A], RTDomainOnly[dt.type], DFC): REG_DIN[T] =
      new REG_DIN[T](DFError.REG_DIN(dfVar.asVarOf[T]))
  end extension
  extension [T <: NonEmptyTuple](dfVarTuple: T)
    def :=[R](rhs: Exact[R])(using
        vt: VarsTuple[T]
    )(using tc: DFVal.TC[DFBits[vt.Width], R], dfc: DFC): Unit = trydf:
      import dfc.getSet
      given dfcAnon: DFC = dfc.anonymize
      def flattenDFValTuple(tpl: Tuple): List[ir.DFVal] =
        tpl.toList.flatMap {
          case dfVal: DFValAny => Some(dfVal.asIR)
          case tpl: Tuple      => flattenDFValTuple(tpl)
        }
      def flattenConcatArgs(arg: ir.DFVal): List[ir.DFVal] =
        import dfc.getSet
        arg match
          case func: ir.DFVal.Func if func.op == FuncOp.++ && func.isAnonymous =>
            func.args.flatMap(ar => flattenConcatArgs(ar.get))
          case _ => List(arg)
      @tailrec def assignRecur(
          // the remaining variables to assign to from most-significant to least
          dfVars: List[ir.DFVal],
          // the remaining arguments to read for assignment from most-significant to least
          args: List[ir.DFVal],
          // the most-significant bits width read so far from the left-most argument
          argReadWidth: Int,
          // the current accumulated concatenated arguments in reverse order
          concat: List[ir.DFVal]
      ): Unit =
        dfVars match
          case dfVar :: nextVars =>
            val concatWidth = concat.map(_.width).sum
            val missingConcatWidth = dfVar.width - concatWidth
            assert(missingConcatWidth >= 0)
            // widths match so we can assign
            if (missingConcatWidth == 0)
              // only one element in concatenation, so there is no need for concatenation
              // and we assign it directly.
              val concatVal =
                if (concat.size == 1) concat.head.asValAny
                else
                  DFVal.Func(DFBits(dfVar.dfType.width), FuncOp.++, concat.reverse)
              // non-bits variables need to be casted to
              val assignVal = dfVar.dfType match
                // no need to cast
                case _: ir.DFBits => concatVal
                // casting required
                case dfType => DFVal.Alias.AsIs.forced(dfType, concatVal.asIR).asValAny
              dfVar.asValAny.assign(assignVal)
              assignRecur(nextVars, args, argReadWidth, Nil)
            // missing more bits to complete assignment, so moving arg element into concat list
            else
              val arg = args.head
              val argLeftoverWidth = arg.width - argReadWidth
              val extraWidth = argLeftoverWidth - missingConcatWidth
              if (extraWidth <= 0)
                val concatArg =
                  // the entire arg should be used
                  if (argReadWidth == 0) arg
                  // partially reading the leftover arg
                  else DFVal.Alias.ApplyRange.forced(arg, argLeftoverWidth - 1, 0)
                // moving the head argument and stacking it in `concat`.
                // we use the concat in reverse later on.
                assignRecur(dfVars, args.drop(1), 0, concatArg :: concat)
              // with the current arg, there will be too many bits are in concat, so we need to
              // split the head and move the extra bits to the args list
              else
                val concatArg = DFVal.Alias.ApplyRange.forced(
                  arg,
                  argLeftoverWidth - 1,
                  extraWidth
                )
                // the args order are from msbits to lsbits, so when we end up with extra bits
                // those will be the lsbits that to be given back to the args list and just the
                // remaining msbits are placed in the concat list
                assignRecur(dfVars, args, argReadWidth + concatArg.width, concatArg :: concat)
              end if
            end if
          case Nil => // done!
      val dfVarsIR = flattenDFValTuple(dfVarTuple)
      val width =
        dfVarsIR.map(_.asValAny.widthIntParam).reduce(_ + _).asInstanceOf[IntParam[vt.Width]]
      val argsIR = flattenConcatArgs(tc(DFBits(width), rhs).asIR)
      val argsBitsIR = argsIR.map { arg =>
        arg.dfType match
          case _: ir.DFBits => arg
          case dfType       => DFVal.Alias.AsIs.forced(ir.DFBits(dfType.width), arg)
      }
      assignRecur(dfVarsIR, argsBitsIR, 0, Nil)
end DFVarOps

object DFPortOps:
  extension [T <: DFTypeAny, C](dfPort: DFVal[T, Modifier[Any, C, Any, Any]])
    def <>[R](rhs: Exact[R])(using
        connectableOnly: AssertGiven[
          C <:< Modifier.Connectable,
          "The LHS of a connection must be a connectable DFHDL value (var/port)."
        ],
        tc: DFVal.TC[T, R],
        dfc: DFC
    ): ConnectPlaceholder =
      if (rhs.value equals r__OPEN) dfPort.connect(DFVal.Open(dfPort.dfType))
      else trydf { dfPort.connect(tc(dfPort.dfType, rhs)) }
      ConnectPlaceholder
end DFPortOps
