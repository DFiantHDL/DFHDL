package dfhdl.core
import dfhdl.compiler.ir
import dfhdl.internals.*
import ir.DFVal.Func.Op as FuncOp
import ir.DFVal.Alias.History.Op as HistoryOp

import scala.annotation.unchecked.uncheckedVariance
import scala.annotation.{implicitNotFound, targetName}
import scala.quoted.*
import DFOpaque.Abstract as DFOpaqueA
import dfhdl.compiler.ir.MemberGetSet
import dfhdl.compiler.printing.{DefaultPrinter, Printer}
import scala.annotation.tailrec

import scala.reflect.ClassTag
trait DFVal[+T <: DFTypeAny, +M <: ModifierAny] extends Any with DFMember[ir.DFVal] with Selectable:

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
  )(using DFC): DFValOf[DFBool] = ${
    DFVal.equalityMacro[T, R, FuncOp.===.type]('this, 'that)
  }
  transparent inline def !=[R](
      inline that: R
  )(using DFC): DFValOf[DFBool] = ${
    DFVal.equalityMacro[T, R, FuncOp.=!=.type]('this, 'that)
  }
end DFVal

type DFValAny = DFVal[DFTypeAny, ModifierAny]
type DFVarAny = DFVal[DFTypeAny, Modifier[Modifier.Assignable, Modifier.Connectable, Any]]
type DFValOf[+T <: DFTypeAny] = DFVal[T, ModifierAny]
type DFVarOf[+T <: DFTypeAny] = DFVal[T, Modifier[Modifier.Assignable, Any, Any]]
type DFPortOf[+T <: DFTypeAny] = DFVal[T, Modifier.Port]

sealed trait TOKEN
type <>[T <: DFType.Supported | Int, M] = T match
  case DFType.Supported =>
    M match
      case VAL   => DFC ?=> DFValOf[DFType.Of[T]]
      case TOKEN => DFToken[DFType.Of[T]]
      case FIELD => DFValOf[DFType.Of[T]]
  // Int is also special cased by the compiler plugin
  case Int => DFVector.ComposedModifier[T, M]

type X[T <: DFType.Supported, M] = M match
  case DFVector.ComposedModifier[d, m] => <>[DFVector[DFType.Of[T], Tuple1[d]], m]
  case Int                             => DFVector[DFType.Of[T], Tuple1[M]]
type JUSTVAL[T <: DFType.Supported] = <>[T, FIELD]

extension (dfVal: ir.DFVal)
  inline def asVal[T <: DFTypeAny, M <: ModifierAny]: DFVal[T, M] =
    DFVal[T, M](dfVal)
  inline def asValOf[T <: DFTypeAny]: DFValOf[T] =
    DFVal[T, ModifierAny](dfVal)
  inline def asValAny: DFValAny =
    DFVal[DFTypeAny, ModifierAny](dfVal)
  inline def asVarOf[T <: DFTypeAny]: DFVarOf[T] =
    DFVal[T, Modifier.VAR](dfVal)
  inline def asVarAny: DFVarAny =
    DFVal[DFTypeAny, Modifier.VAR](dfVal)

extension (dfVal: DFValAny)
  inline def asVal[T <: DFTypeAny, M <: ModifierAny]: DFVal[T, M] =
    dfVal.asInstanceOf[DFVal[T, M]]
  inline def asValOf[T <: DFTypeAny]: DFValOf[T] =
    dfVal.asInstanceOf[DFVal[T, ModifierAny]]
  inline def asVarOf[T <: DFTypeAny]: DFVarOf[T] =
    dfVal.asInstanceOf[DFVal[T, Modifier.VAR]]
  inline def asVarAny: DFVarAny =
    dfVal.asInstanceOf[DFVal[DFTypeAny, Modifier.VAR]]

object DFVal:
  final class Final[+T <: DFTypeAny, +M <: ModifierAny](val irValue: ir.DFVal | DFError)
      extends AnyVal
      with DFVal[T, M]
  def apply[T <: DFTypeAny, M <: ModifierAny](irValue: ir.DFVal | DFError): DFVal[T, M] =
    new Final[T, M](irValue)
  inline def unapply(arg: DFValAny): Option[ir.DFVal] = Some(arg.asIR)
  object OrTupleOrStruct:
    def unapply(arg: Any)(using DFC): Option[DFValAny] =
      arg match
        case dfVal: DFValAny     => Some(dfVal)
        case DFTuple.Val(dfVal)  => Some(dfVal)
        case DFStruct.Val(dfVal) => Some(dfVal)
        case _                   => None

  trait Refiner[T <: FieldsOrTuple, A, I]:
    type Out <: DFVal[DFStruct[T], Modifier[A, Any, I]]
  object Refiner:
    transparent inline given [T <: FieldsOrTuple, A, I]: Refiner[T, A, I] = ${
      refineMacro[T, A, I]
    }
    def refineMacro[T <: FieldsOrTuple, A, I](using
        Quotes,
        Type[T],
        Type[A],
        Type[I]
    ): Expr[Refiner[T, A, I]] =
      import quotes.reflect.*
      val dfValTpe = TypeRepr.of[DFVal[DFStruct[T], Modifier[A, Any, I]]]
      val tTpe = TypeRepr.of[T]
      val fields: List[(String, TypeRepr)] = tTpe.asTypeOf[Any] match
        case '[NonEmptyTuple] =>
          tTpe.getTupleArgs.zipWithIndex.map((f, i) =>
            f.asTypeOf[Any] match
              case '[DFValOf[t]] =>
                (s"_${i + 1}", TypeRepr.of[DFVal[t, Modifier[A, Any, I]]])
          )
        case _ =>
          val clsSym = tTpe.classSymbol.get
          clsSym.caseFields.map(m =>
            tTpe.memberType(m).asTypeOf[Any] match
              case '[DFValOf[t]] =>
                (m.name.toString, TypeRepr.of[DFVal[t, Modifier[A, Any, I]]])
          )

      val refined = fields.foldLeft(dfValTpe) { case (r, (n, t)) =>
        Refinement(r, n, t)
      }
      val refinedType = refined.asTypeOf[DFVal[DFStruct[T], Modifier[A, Any, I]]]
      '{
        new Refiner[T, A, I]:
          type Out = refinedType.Underlying
      }
    end refineMacro
  end Refiner

  def equalityMacro[T <: DFTypeAny, R, Op <: FuncOp](
      dfVal: Expr[DFValOf[T]],
      arg: Expr[R]
  )(using Quotes, Type[T], Type[R], Type[Op]): Expr[DFValOf[DFBool]] =
    import quotes.reflect.*
    if (TypeRepr.of[T].typeSymbol equals defn.NothingClass) return '{
      compiletime.error("This is fake")
    }
    val exact = arg.asTerm.exactTerm
    val exactExpr = exact.asExpr
    val exactType = exact.tpe.asTypeOf[Any]
    '{
      val c = compiletime.summonInline[
        DFVal.Compare[T, exactType.Underlying, Op, false]
      ]
      val dfc = compiletime.summonInline[DFC]
      trydf {
        c($dfVal, $exactExpr)(using
          dfc,
          compiletime.summonInline[ValueOf[Op]],
          new ValueOf[false](false)
        )
      }(using dfc)
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

  trait InitCheck[I]
  given [I](using
      initializableOnly: AssertGiven[
        I =:= Modifier.Initializable,
        "Can only initialize a DFHDL port or variable that are not already initialized."
      ]
  ): InitCheck[I] with {}

  extension [T <: DFTypeAny, M <: ModifierAny](dfVal: DFVal[T, M])
    def tag[CT <: ir.DFTag: ClassTag](customTag: CT)(using
        dfc: DFC
    ): DFVal[T, M] =
      import dfc.getSet
      dfVal.asIR
        .setTags(_.tag(customTag))
        .setMeta(m => if (m.isAnonymous && !dfc.getMeta.isAnonymous) dfc.getMeta else m)
        .asVal[T, M]
    def tag[CT <: ir.DFTag: ClassTag](condCustomTag: Conditional[CT])(using
        dfc: DFC
    ): DFVal[T, M] = if (condCustomTag.isActive) dfVal.tag(condCustomTag.getArg) else dfVal
    def hasTag[CT <: ir.DFTag: ClassTag](using dfc: DFC): Boolean =
      import dfc.getSet
      dfVal.asIR.tags.hasTagOf[CT]
    def setName(name: String)(using dfc: DFC): DFVal[T, M] =
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
        case dcl: ir.DFVal.Dcl => dfVal
        case dfValIR           => dfValIR.setMeta(m => m.anonymize).asVal[T, M]
  end extension

  case object ExtendTag extends ir.DFTagOf[ir.DFVal]
  type ExtendTag = ExtendTag.type
  case object TruncateTag extends ir.DFTagOf[ir.DFVal]
  type TruncateTag = TruncateTag.type

  extension [T <: DFTypeAny, A, C, I, R](dfVal: DFVal[T, Modifier[A, C, I]])
    private[core] def initForced(tokens: List[ir.DFTokenAny])(using
        dfc: DFC
    ): DFVal[T, Modifier[A, C, Modifier.Initialized]] =
      import dfc.getSet
      require(
        dfVal.asIR.isAnonymous,
        s"Cannot initialize a named value ${dfVal.asIR.getFullName}. Initialization is only supported at the declaration of the value."
      )
      dfVal.tag(ir.ExternalInit(tokens)).asVal[T, Modifier[A, C, Modifier.Initialized]]

    def init(
        tokenValues: DFToken.Value[T]*
    )(using InitCheck[I], DFC): DFVal[T, Modifier[A, C, Modifier.Initialized]] = trydf {
      val tvList = tokenValues.view.filter(_.enable).map(tv => tv(dfVal.dfType).asIR).toList
      if (tvList.isEmpty) dfVal.asVal[T, Modifier[A, C, Modifier.Initialized]]
      else initForced(tvList)
    }
  end extension
  extension [T <: NonEmptyTuple, A, C, I](dfVal: DFVal[DFTuple[T], Modifier[A, C, I]])
    def init(
        tokenValues: DFToken.TupleValues[T]
    )(using InitCheck[I], DFC): DFVal[DFTuple[T], Modifier[A, C, Modifier.Initialized]] = trydf {
      if (tokenValues.enable) dfVal.initForced(tokenValues(dfVal.dfType).map(_.asIR))
      else dfVal.asVal[DFTuple[T], Modifier[A, C, Modifier.Initialized]]
    }

  implicit def BooleanHack(from: DFValOf[DFBoolOrBit])(using DFC): Boolean =
    ???
  implicit inline def DFValConversionExact[T <: DFTypeAny, R <: ExactTypes](
      from: R
  )(using dfType: T, es: Exact.Summon[R, from.type])(using
      tc: TC[T, es.Out],
      dfc: DFC
  ): DFValOf[T] = trydf { tc(dfType, es(from)) }

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
  implicit def DFOpaqueValConversion[T <: DFOpaque.Abstract, R <: DFOpaque.Abstract](
      from: DFValOf[DFOpaque[R]]
  )(using DFC, R <:< T): DFValOf[DFOpaque[T]] = from.asInstanceOf[DFValOf[DFOpaque[T]]]

  implicit def DFValConversion[T <: DFTypeAny, R](
      from: R
  )(using dfType: T)(using
      tc: TC[T, R],
      dfc: DFC
  ): DFValOf[T] = trydf { tc(dfType, from) }

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
    def apply[T <: DFTypeAny, M <: ModifierAny](dfType: T, modifier: M)(using
        DFC
    ): DFVal[T, M] =
      ir.DFVal
        .Dcl(
          dfType.asIR,
          modifier.asIR,
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
      def apply[AT <: DFTypeAny, VT <: DFTypeAny, M <: ModifierAny](
          aliasType: AT,
          relVal: DFVal[VT, M],
          tokenFunc: DFToken[VT] => DFToken[AT],
          forceNewAlias: Boolean = false
      )(using DFC): DFVal[AT, M] =
        relVal.asIR match
          // anonymous constant are replace by a different constant
          // after its token value was converted according to the alias
          case const: ir.DFVal.Const if const.isAnonymous && !forceNewAlias =>
            val updatedToken = tokenFunc(const.token.asTokenOf[VT])
            Const(updatedToken).asVal[AT, M]
          // named constants or other non-constant values are referenced
          // in a new alias construct
          case _ =>
            forced(aliasType.asIR, relVal.asIR).asVal[AT, M]
      end apply
      def forced(aliasType: ir.DFType, relVal: ir.DFVal)(using DFC): ir.DFVal =
        lazy val alias: ir.DFVal.Alias.AsIs =
          ir.DFVal.Alias.AsIs(
            aliasType,
            relVal.refTW(alias),
            dfc.owner.ref,
            dfc.getMeta,
            ir.DFTags.empty
          )
        alias.addMember
      def ident[T <: DFTypeAny, M <: ModifierAny](relVal: DFVal[T, M])(using
          DFC
      ): DFVal[T, M] =
        import ir.DFVal.Alias.IdentTag
        apply(relVal.dfType, relVal, x => x, forceNewAlias = true).tag(IdentTag)
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
          initOption: Option[DFToken[T]]
      )(using DFC): DFValOf[T] =
        lazy val alias: ir.DFVal.Alias.History =
          ir.DFVal.Alias.History(
            relVal.dfType.asIR,
            relVal.asIR.refTW(alias),
            step,
            op,
            initOption.map(_.asIR),
            dfc.owner.ref,
            dfc.getMeta,
            ir.DFTags.empty
          )
        alias.addMember.asValOf[T]
      end apply
    end History
    object ApplyRange:
      def apply[W <: Int, M <: ModifierAny, H <: Int, L <: Int](
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
          // after its token value was converted according to the alias
          case const: ir.DFVal.Const if const.isAnonymous =>
            import DFBits.Token.Ops.apply
            val updatedToken = const.token.asTokenOf[DFBits[Int]](relBitHigh, relBitLow)
            Const(updatedToken).asIR
          // named constants or other non-constant values are referenced
          // in a new alias construct
          case _ =>
            lazy val alias: ir.DFVal.Alias.ApplyRange =
              ir.DFVal.Alias.ApplyRange(
                relVal.refTW(alias),
                relBitHigh,
                relBitLow,
                dfc.owner.ref,
                dfc.getMeta,
                ir.DFTags.empty
              )
            alias.addMember
      end forced
    end ApplyRange
    object ApplyIdx:
      def apply[T <: DFTypeAny, W <: Int, M <: ModifierAny, IW <: Int](
          dfType: T,
          relVal: DFVal[DFTypeAny, M],
          relIdx: DFValOf[DFUInt[IW]]
      )(using DFC): DFVal[T, M] =
        lazy val alias: ir.DFVal.Alias.ApplyIdx =
          ir.DFVal.Alias.ApplyIdx(
            dfType.asIR,
            relVal.asIR.refTW(alias),
            relIdx.asIR.refTW(alias),
            dfc.owner.ref,
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
            lazy val alias: ir.DFVal.Alias.SelectField =
              ir.DFVal.Alias.SelectField(
                dfTypeIR,
                relValIR.refTW(alias),
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
    type Out = DFValOf[T]
    type Ctx = DFC
    final def apply(dfType: T, value: R)(using DFC): Out = conv(dfType, value)

  trait TCLP:
    // Accept OPEN in compile-time, but throw exception where it should not be used
    given fromOPEN[T <: DFTypeAny]: TC[T, ir.OpenConnectTag] with
      def conv(dfType: T, value: ir.OpenConnectTag)(using Ctx): DFValOf[T] =
        throw new IllegalArgumentException("OPEN cannot be used here")
    // Accept any bubble value
    given fromBubble[T <: DFTypeAny, V <: Bubble](using
        tokenTC: DFToken.TC[T, V]
    ): TC[T, V] with
      def conv(dfType: T, value: V)(using Ctx): DFValOf[T] =
        Const(tokenTC(dfType, value))
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
    given sameValType[T <: DFTypeAny, V <: DFValOf[T]]: TC[T, V] with
      def conv(dfType: T, value: V)(using Ctx): DFValOf[T] =
        given Printer = DefaultPrinter
        given MemberGetSet = dfc.getSet
        require(
          dfType == value.dfType,
          s"Unsupported value of type `${value.dfType.codeString}` for DFHDL receiver type `${dfType.codeString}`."
        )
        value
    given sameValAndTokenType[T <: DFTypeAny, V <: T <> TOKEN]: TC[T, V] with
      def conv(dfType: T, value: V)(using Ctx): DFValOf[T] =
        given Printer = DefaultPrinter
        given MemberGetSet = dfc.getSet
        require(
          dfType == value.dfType,
          s"Unsupported value of type `${value.dfType.codeString}` for DFHDL receiver type `${dfType.codeString}`."
        )
        DFVal.Const(value)
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
    type Out = DFValOf[T]
    type Ctx = DFC
    final protected def func(arg1: DFValAny, arg2: DFValAny)(using
        DFC,
        ValueOf[Op],
        ValueOf[C]
    ): DFValOf[DFBool] =
      val list = if (valueOf[C]) List(arg2, arg1) else List(arg1, arg2)
      DFVal.Func(DFBool, valueOf[Op], list)
    def apply(dfVal: DFValOf[T], arg: V)(using
        DFC,
        ValueOf[Op],
        ValueOf[C]
    ): DFValOf[DFBool] =
      val dfValArg = conv(dfVal.dfType, arg)
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
    given sameValType[T <: DFTypeAny, R <: DFValOf[T], Op <: FuncOp, C <: Boolean](using
        ValueOf[Op],
        ValueOf[C]
    ): Compare[T, R, Op, C] with
      def conv(dfType: T, arg: R)(using Ctx): DFValOf[T] =
        given Printer = DefaultPrinter(using dfc.getSet)
        require(
          dfType == arg.dfType,
          s"Cannot compare DFHDL value type `${dfType.codeString}` with DFHDL value type `${arg.dfType.codeString}`."
        )
        arg
    given sameValAndTokenType[
        T <: DFTypeAny,
        R <: T <> TOKEN,
        Op <: FuncOp,
        C <: Boolean
    ](using
        ValueOf[Op],
        ValueOf[C]
    ): Compare[T, R, Op, C] with
      def conv(dfType: T, arg: R)(using Ctx): DFValOf[T] =
        given Printer = DefaultPrinter(using dfc.getSet)
        require(
          dfType == arg.dfType,
          s"Cannot compare DFHDL value type `${dfType.codeString}` with DFHDL value type `${arg.dfType.codeString}`."
        )
        DFVal.Const(arg)
    end sameValAndTokenType
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

//  object Conversions:
//    implicit transparent inline def fromArg[T <: DFTypeAny, R](
//        inline arg: R
//    ): DFValOf[T] = ${ fromArgMacro[T]('arg) }

  trait DFDomainOnly
  given (using domain: DFC.Domain)(using
      AssertGiven[
        domain.type <:< DFC.Domain.DF,
        "This construct is only available in a dataflow domain."
      ]
  ): DFDomainOnly with {}
  trait RTDomainOnly
  given (using domain: DFC.Domain)(using
      AssertGiven[
        domain.type <:< DFC.Domain.RT,
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
  object Ops:
    // TODO: change to step Inlined[S] for all operations after https://github.com/lampepfl/dotty/issues/14451
    // is resolved.
    extension [T <: DFTypeAny, A, C, I, S <: Int, V](dfVal: DFVal[T, Modifier[A, C, I]])
      def prev(step: Inlined[S], init: Exact[V])(using
          dfc: DFC,
          dfOnly: DFDomainOnly,
          tokenTC: DFToken.TC[T, V],
          check: Arg.Positive.Check[S]
      ): DFValOf[T] = trydf {
        check(step)
        val initOpt = Some(tokenTC(dfVal.dfType, init))
        DFVal.Alias.History(dfVal, step, HistoryOp.Prev, initOpt)
      }
      def prev(step: Inlined[S])(using
          dfc: DFC,
          dfOnly: DFDomainOnly,
          initCheck: PrevInitCheck[I],
          check: Arg.Positive.Check[S]
      ): DFValOf[T] = trydf {
        check(step)
        DFVal.Alias.History(dfVal, step, HistoryOp.Prev, None)
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
          Some(DFToken.bubble(dfVal.dfType)) // pipe always has a bubble for initialization
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
        DFVal.Alias.History(dfVal, step, HistoryOp.Reg, None)
      }
      def reg(step: Inlined[S], init: Exact[V])(using
          dfc: DFC,
          rtOnly: RTDomainOnly,
          tokenTC: DFToken.TC[T, V],
          check: Arg.Positive.Check[S]
      ): DFValOf[T] = trydf {
        check(step)
        val initOpt = Some(tokenTC(dfVal.dfType, init))
        DFVal.Alias.History(dfVal, step, HistoryOp.Reg, initOpt)
      }
      inline def reg(using DFC, RTDomainOnly, RegInitCheck[I]): DFValOf[T] = dfVal.reg(1)
    end extension

    extension [T <: DFTypeAny, A, C, I](dfVal: DFVal[T, Modifier[A, C, I]])
      def bits(using w: Width[T])(using DFC): DFValOf[DFBits[w.Out]] = trydf {
        import DFToken.Ops.{bits => bitsDFToken}
        DFVal.Alias.AsIs(DFBits(dfVal.width), dfVal, _.bitsDFToken)
      }
      def genNewVar(using DFC): DFVarOf[T] = trydf {
        DFVal.Dcl(dfVal.dfType, Modifier.VAR)
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

protected trait VarsTuple[T <: NonEmptyTuple]:
  type Width <: Int
protected object VarsTuple:
  transparent inline given [T <: NonEmptyTuple]: VarsTuple[T] = ${ evMacro[T] }
  def evMacro[T <: NonEmptyTuple](using Quotes, Type[T]): Expr[VarsTuple[T]] =
    import quotes.reflect.*
    val tTpe = TypeRepr.of[T]
    def varsCheck(tpe: TypeRepr): Option[String] =
      tpe.asTypeOf[Any] match
        case '[DFVarOf[t]] => None
        case '[NonEmptyTuple] =>
          val AppliedType(_, tArgs) = tpe: @unchecked
          tArgs.view.map(varsCheck).collectFirst { case Some(v) => v }
        case _ =>
          Some(s"All tuple elements must be mutable but found an immutable type `${tpe.showType}`")
    varsCheck(tTpe) match
      case Some(err) => '{ compiletime.error(${ Expr(err) }) }
      case None =>
        import Width.calcValWidth
        val widthType = tTpe.calcValWidth(false).asTypeOf[Int]
        '{
          new VarsTuple[T]:
            type Width = widthType.Underlying
        }
  end evMacro
end VarsTuple

object DFVarOps:
  protected type VarOnly[A] = AssertGiven[
    A <:< Modifier.Assignable,
    "Cannot assign to an immutable value."
  ]
  protected type RegOnly[A] = AssertGiven[
    A <:< Modifier.RegRef,
    "Can only reference `din` of a register. This value is not a register."
  ]
  protected type LocalOrNonED[A] = AssertGiven[
    (A <:< DFC.Scope.Process) | util.NotGiven[A <:< DFC.Domain.ED],
    "Blocking assignment `:=` is not allowed for a non-local variable in this domain.\nChange the assignment to a non-blocking assignment `:==` or the position of the defined variable."
  ]
  protected type NotLocalVar[A] = AssertGiven[
    util.NotGiven[A <:< DFC.Scope.Process],
    "Non-blocking assignment `:==` is not allowed for a local variable (defined inside the process block).\nChange the assignment to a blocking assignment `:=` or the position of the defined variable."
  ]
  protected type EDDomainOnly[A] = AssertGiven[
    A <:< DFC.Domain.ED,
    "Non-blocking assignment `:==` is allowed only inside an event-driven (ED) domain.\nChange the assignment to a regular assignment `:=` or the logic domain to ED."
  ]
  protected type InsideProcess[A] = AssertGiven[
    DFC.Scope.Process | util.NotGiven[A <:< DFC.Domain.ED],
    "Assignments `:=`/`:==` are only allowed inside a process under an event-driven (ED) domain.\nChange the assignment to a connection `<>` or place it in a process."
  ]
  extension [T <: DFTypeAny, A, C, I](dfVar: DFVal[T, Modifier[A, C, I]])
    def :=[R](rhs: Exact[R])(using
        varOnly: VarOnly[A],
//        localOrNonED: LocalOrNonED[A],
        insideProcess: InsideProcess[A],
        tc: DFVal.TC[T, R],
        dfc: DFC
    ): Unit = trydf {
      dfVar.assign(tc(dfVar.dfType, rhs))
    }
    def :==[R](rhs: Exact[R])(using dt: DFC.Domain)(using
        varOnly: VarOnly[A],
        edDomainOnly: EDDomainOnly[dt.type],
//        notLocalVar: NotLocalVar[A],
        insideProcess: InsideProcess[A],
        tc: DFVal.TC[T, R],
        dfc: DFC
    ): Unit = trydf {
      dfVar.nbassign(tc(dfVar.dfType, rhs))
    }
  end extension
  extension [T <: NonEmptyTuple](dfVarTuple: T)
    def :=[R](rhs: Exact[R])(using
        vt: VarsTuple[T]
    )(using tc: DFVal.TC[DFBits[vt.Width], R], dfc: DFC): Unit = trydf:
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
      val width = Inlined.forced[vt.Width](dfVarsIR.map(_.dfType.width).sum)
      val argsIR = flattenConcatArgs(tc(DFBits(width), rhs).asIR)
      val argsBitsIR = argsIR.map { arg =>
        arg.dfType match
          case _: ir.DFBits => arg
          case dfType       => DFVal.Alias.AsIs.forced(ir.DFBits(dfType.width), arg)
      }
      assignRecur(dfVarsIR, argsBitsIR, 0, Nil)
end DFVarOps

object DFPortOps:
  extension [T <: DFTypeAny, A, C, I](dfPort: DFVal[T, Modifier[A, C, I]])
    def <>[R](rhs: Exact[R])(using
        connectableOnly: AssertGiven[
          C <:< Modifier.Connectable,
          "The LHS of a connection must be a connectable DFHDL value (var/port)."
        ],
        tc: DFVal.TC[T, R],
        dfc: DFC
    ): Unit =
      if (rhs.value equals ir.OpenConnectTag) dfPort.tag(ir.OpenConnectTag)
      else trydf { dfPort.connect(tc(dfPort.dfType, rhs)) }
end DFPortOps
