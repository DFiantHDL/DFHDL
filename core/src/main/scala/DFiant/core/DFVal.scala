package DFiant.core
import DFiant.compiler.ir
import DFiant.internals.*
import DFiant.compiler.ir.DFVal.{Modifier, ModifierAny}
import ir.DFVal.Func.Op as FuncOp

import scala.annotation.unchecked.uncheckedVariance
import scala.annotation.{implicitNotFound, targetName}
import scala.quoted.*
import DFOpaque.Abstract as DFOpaqueA
import DFiant.compiler.ir.MemberGetSet
import DFiant.compiler.printing.{DefaultPrinter, Printer}

import scala.reflect.ClassTag
final class DFVal[+T <: DFTypeAny, +M <: ModifierAny](val value: ir.DFVal)
    extends AnyVal
    with DFMember[ir.DFVal]
    with Selectable:

  def selectDynamic(name: String)(using DFC): Any =
    val ir.DFStruct(structName, fieldMap) = value.dfType
    val dfType = fieldMap(name)
    DFVal.Alias
      .SelectField(this, name)
      .asIR
      .asVal[DFTypeAny, ModifierAny]

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
  inline def unapply(arg: DFValAny): Option[ir.DFVal] = Some(arg.value)
  object OrTupleOrStruct:
    def unapply(arg: Any)(using DFC): Option[DFValAny] =
      arg match
        case dfVal: DFValAny     => Some(dfVal)
        case DFTuple.Val(dfVal)  => Some(dfVal)
        case DFStruct.Val(dfVal) => Some(dfVal)
        case _                   => None

  trait Refiner[T <: FieldsOrTuple, A]:
    type Out <: DFVal[DFStruct[T], Modifier[A, Any, Any]]
  object Refiner:
    transparent inline given [T <: FieldsOrTuple, A]: Refiner[T, A] = ${
      refineMacro[T, A]
    }
    def refineMacro[T <: FieldsOrTuple, A](using
        Quotes,
        Type[T],
        Type[A]
    ): Expr[Refiner[T, A]] =
      import quotes.reflect.*
      val dfValTpe = TypeRepr.of[DFVal[DFStruct[T], Modifier[A, Any, Any]]]
      val tTpe = TypeRepr.of[T]
      val fields: List[(String, TypeRepr)] = tTpe.asTypeOf[Any] match
        case '[NonEmptyTuple] =>
          tTpe.getTupleArgs.zipWithIndex.map((f, i) =>
            f.asTypeOf[Any] match
              case '[DFValOf[t]] =>
                (s"_${i + 1}", TypeRepr.of[DFVal[t, Modifier[A, Any, Any]]])
          )
        case _ =>
          val clsSym = tTpe.classSymbol.get
          clsSym.caseFields.map(m =>
            tTpe.memberType(m).asTypeOf[Any] match
              case '[DFValOf[t]] =>
                (m.name.toString, TypeRepr.of[DFVal[t, Modifier[A, Any, Any]]])
          )

      val refined = fields.foldLeft(dfValTpe) { case (r, (n, t)) =>
        Refinement(r, n, t)
      }
      val refinedType = refined.asTypeOf[DFVal[DFStruct[T], Modifier[A, Any, Any]]]
      '{
        new Refiner[T, A]:
          type Out = refinedType.Underlying
      }
    end refineMacro
  end Refiner

  inline implicit def refined[T <: FieldsOrTuple, A](
      inline dfVal: DFVal[DFStruct[T], Modifier[A, Any, Any]]
  )(using
      r: Refiner[T, A]
  ): r.Out = dfVal.asInstanceOf[r.Out]

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
      c($dfVal, $exactExpr)(using
        compiletime.summonInline[DFC],
        compiletime.summonInline[ValueOf[Op]],
        new ValueOf[false](false)
      )
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

  final val Modifier = DFiant.compiler.ir.DFVal.Modifier
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

type DFValAny = DFVal[DFTypeAny, ModifierAny]
type DFVarAny = DFVal[DFTypeAny, VAR]
type DFValOf[+T <: DFTypeAny] = DFVal[T, ModifierAny]
type DFVarOf[+T <: DFTypeAny] = DFVal[T, VAR]
type DFPortOf[+T <: DFTypeAny] = DFVal[T, Modifier.Port]

val IN = Modifier.IN
val OUT = Modifier.OUT
private val INOUT = Modifier.INOUT
val VAR = Modifier.VAR
type VAL = Modifier.VAL
type VAR = Modifier.VAR.type
type IN = Modifier.IN.type
type OUT = Modifier.OUT.type
private type INOUT = Modifier.INOUT.type
sealed trait TOKEN
type <>[T <: DFType.Supported, M] = M match
  case VAL              => DFValOf[DFType.Of[T]]
  case TOKEN            => DFToken[DFType.Of[T]]
  case VAR              => DFVarOf[DFType.Of[T]]
  case IN | OUT | INOUT => DFPortOf[DFType.Of[T]]
type JUSTVAL[T <: DFType.Supported] = <>[T, VAL]

extension (dfVal: ir.DFVal)
  inline def asVal[T <: DFTypeAny, M <: ModifierAny]: DFVal[T, M] =
    DFVal[T, M](dfVal)
  inline def asValOf[T <: DFTypeAny]: DFValOf[T] =
    DFVal[T, ModifierAny](dfVal)
  inline def asValAny: DFValAny =
    DFVal[DFTypeAny, ModifierAny](dfVal)
  inline def asVarOf[T <: DFTypeAny]: DFVarOf[T] =
    DFVal[T, VAR](dfVal)
  inline def asVarAny: DFVarAny =
    DFVal[DFTypeAny, VAR](dfVal)
  inline def asPortOf[T <: DFTypeAny]: DFPortOf[T] =
    DFVal[T, Modifier.Port](dfVal)

private object CompanionsDFVal:
  trait InitCheck[I]
  given [I](using
      initializableOnly: AssertGiven[
        I =:= Modifier.Initializable,
        "Can only initialize a dataflow port or variable that are not already initialized."
      ]
  ): InitCheck[I] with {}
  object Extensions:
    extension [T <: DFTypeAny, M <: ModifierAny](dfVal: DFVal[T, M])
      def tag[CT <: ir.DFTag: ClassTag](customTag: CT)(using
          dfc: DFC
      ): DFVal[T, M] =
        import DFiant.core.tag as tagIR
        dfVal.asIR.tagIR(customTag).asVal[T, M]
    extension [T <: DFTypeAny, A, C, I](dfVal: DFVal[T, Modifier[A, C, I]])
      private[core] def initForced(tokens: List[ir.DFTokenAny])(using
          dfc: DFC
      ): DFVal[T, Modifier[A, C, Modifier.Initialized]] =
        import dfc.getSet
        assert(
          dfVal.asIR.isAnonymous,
          s"Cannot initialize a named value ${dfVal.asIR.getFullName}. Initialization is only supported at the declaration of the value."
        )
        dfVal.tag(ir.ExternalInit(tokens)).asIR.asVal[T, Modifier[A, C, Modifier.Initialized]]

      def init(
          tokenValues: DFToken.Value[T]*
      )(using InitCheck[I], DFC): DFVal[T, Modifier[A, C, Modifier.Initialized]] =
        initForced(tokenValues.view.map(tv => tv(dfVal.dfType).asIR).toList)
    end extension
    extension [T <: NonEmptyTuple, A, C, I](dfVal: DFVal[DFTuple[T], Modifier[A, C, I]])
      def init(
          tokenValues: DFToken.TupleValues[T]
      )(using InitCheck[I], DFC): DFVal[DFTuple[T], Modifier[A, C, Modifier.Initialized]] =
        dfVal.initForced(tokenValues(dfVal.dfType).map(_.asIR))
  end Extensions

  object Conversions:
    implicit def BooleanHack(from: DFValOf[DFBoolOrBit])(using DFC): Boolean =
      ???
    implicit inline def DFValConversionExact[T <: DFTypeAny, R <: ExactTypes](
        inline from: R
    )(using dfType: T, es: Exact.Summon[R, from.type])(using
        tc: CompanionsDFVal.TC[T, es.Out]
    ): DFValOf[T] = tc(dfType, es(from))
    implicit def DFValConversion[T <: DFTypeAny, R](
        from: R
    )(using dfType: T)(using
        tc: CompanionsDFVal.TC[T, R]
    ): DFValOf[T] = tc(dfType, from)

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
            Const(updatedToken).asIR.asVal[AT, M]
          // named constants or other non-constant values are referenced
          // in a new alias construct
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
      def ident[T <: DFTypeAny, M <: ModifierAny](relVal: DFVal[T, M])(using
          DFC
      ): DFVal[T, M] =
        apply(relVal.dfType, relVal, x => x, forceNewAlias = true)
      def bind[T <: DFTypeAny, M <: ModifierAny](relVal: DFVal[T, M], bindName: String)(using
          DFC
      ): DFVal[T, M] =
        import ir.DFConditional.DFCaseBlock.Pattern
        ident(relVal)(using dfc.setName(bindName)).tag(Pattern.Bind.Tag)
    end AsIs
    object History:
      export ir.DFVal.Alias.History.Op
      def apply[T <: DFTypeAny](relVal: DFValOf[T], step: Int, op: Op)(using DFC): DFValOf[T] =
        lazy val alias: ir.DFVal =
          ir.DFVal.Alias.History(
            relVal.dfType.asIR,
            relVal.asIR.refTW(alias),
            step,
            op,
            dfc.owner.ref,
            dfc.getMeta,
            ir.DFTags.empty
          )
        alias.addMember.asValOf[T]

    object ApplyRange:
      def apply[W <: Int, M <: ModifierAny, H <: Int, L <: Int](
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
      def apply[T <: DFTypeAny, W <: Int, M <: ModifierAny, IW <: Int](
          dfType: T,
          relVal: DFVal[DFTypeAny, M],
          relIdx: DFUInt[IW] <> VAL
      )(using DFC): DFVal[T, M] =
        lazy val alias: ir.DFVal =
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
        val ir.DFStruct(_, fieldMap) = relValIR.dfType
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
            lazy val alias: ir.DFVal =
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

  trait TC[T <: DFTypeAny, -R] extends TCConv[T, R, DFValAny]:
    type Out = DFValOf[T]
    def apply(dfType: T, value: R): Out = conv(dfType, value)

  trait TCLP:
    // Accept any bubble value
    given fromBubble[T <: DFTypeAny](using
        tokenTC: DFToken.TC[T, Bubble],
        dfc: DFC
    ): TC[T, Bubble] with
      def conv(dfType: T, value: Bubble): DFValOf[T] =
        Const(tokenTC(dfType, value))
    transparent inline given errorDMZ[T <: DFTypeAny, R](using
        t: ShowType[T],
        r: ShowType[R]
    ): TC[T, R] =
      Error.call[
        (
            "Unsupported value of type `",
            r.Out,
            "` for dataflow receiver type `",
            t.Out,
            "`."
        )
      ]
    given sameValType[T <: DFTypeAny](using DFC): TC[T, T <> VAL] with
      def conv(dfType: T, value: T <> VAL): DFValOf[T] =
        given Printer = DefaultPrinter
        given MemberGetSet = dfc.getSet
        assert(
          dfType == value.dfType,
          s"Unsupported value of type `${value.dfType.codeString}` for dataflow receiver type `${dfType.codeString}`."
        )
        value
    given sameValAndTokenType[T <: DFTypeAny](using
        DFC
    ): TC[T, T <> TOKEN] with
      def conv(dfType: T, value: T <> TOKEN): DFValOf[T] =
        given Printer = DefaultPrinter
        given MemberGetSet = dfc.getSet
        assert(
          dfType == value.dfType,
          s"Unsupported value of type `${value.dfType.codeString}` for dataflow receiver type `${dfType.codeString}`."
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
  end TC

  trait Compare[T <: DFTypeAny, -V, Op <: FuncOp, C <: Boolean] extends TCConv[T, V, DFValAny]:
    type Out = DFValOf[T]
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
    def conv(dfType: T, arg: V): DFValOf[T]
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
            "Cannot compare dataflow value of type `",
            t.Out,
            "` with value of type `",
            r.Out,
            "`."
        )
      ]
    inline given sameValType[T <: DFTypeAny, Op <: FuncOp, C <: Boolean](using
        DFC,
        ValueOf[Op],
        ValueOf[C]
    ): Compare[T, T <> VAL, Op, C] with
      def conv(dfType: T, arg: T <> VAL): DFValOf[T] =
        given Printer = DefaultPrinter(using dfc.getSet)
        assert(
          dfType == arg.dfType,
          s"Cannot compare dataflow value type `${dfType.codeString}` with dataflow value type `${arg.dfType.codeString}`."
        )
        arg
    inline given sameValAndTokenType[
        T <: DFTypeAny,
        Op <: FuncOp,
        C <: Boolean
    ](using
        DFC,
        ValueOf[Op],
        ValueOf[C]
    ): Compare[T, T <> TOKEN, Op, C] with
      def conv(dfType: T, arg: T <> TOKEN): DFValOf[T] =
        given Printer = DefaultPrinter(using dfc.getSet)
        assert(
          dfType == arg.dfType,
          s"Cannot compare dataflow value type `${dfType.codeString}` with dataflow value type `${arg.dfType.codeString}`."
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

  trait PrevCheck[I]
  given [I](using
      AssertGiven[
        I =:= Modifier.Initialized,
        "Previous dataflow values can only be summoned for initialized values."
      ]
  ): PrevCheck[I] with {}
  object Ops:
    implicit class __Prev[T <: DFTypeAny, A, C, I](dfVal: DFVal[T, Modifier[A, C, I]]):
      def prev[S <: Int](
          step: Inlined[S]
      )(using
          dfc: DFC,
          prevCheck: PrevCheck[I],
          check: Arg.Positive.Check[S]
      ): DFValOf[T] =
        check(step)
        DFVal.Alias.History(dfVal, step, DFVal.Alias.History.Op.Prev)
      inline def prev(using PrevCheck[I], DFC): DFValOf[T] = dfVal.prev(1)

    extension [T <: DFTypeAny, A, C, I](dfVal: DFVal[T, Modifier[A, C, I]])
      def bits(using w: Width[T])(using DFC): DFValOf[DFBits[w.Out]] =
        import DFToken.Ops.{bits => bitsDFToken}
        DFVal.Alias.AsIs(DFBits(dfVal.width), dfVal, _.bitsDFToken)
      def pipe[S <: Int](
          step: Inlined[S]
      )(using dfc: DFC, check: Arg.Positive.Check[S]): DFValOf[T] =
        check(step)
        DFVal.Alias.History(dfVal, step, DFVal.Alias.History.Op.Pipe)
      inline def pipe(using DFC): DFValOf[T] = dfVal.pipe(1)
      def genNewVar(using DFC): DFVarOf[T] =
        DFVal.Dcl(dfVal.dfType, VAR)
    end extension
  end Ops
end CompanionsDFVal

extension [T <: DFTypeAny](dfVar: DFValOf[T])
  def assign[R <: DFTypeAny](rhs: DFValOf[R])(using DFC): Unit =
    DFNet(dfVar.asIR, DFNet.Op.Assignment, rhs.asIR)

extension [T <: DFTypeAny](lhs: DFValOf[T])
  def connect[R <: DFTypeAny](rhs: DFValOf[R])(using DFC): Unit =
    DFNet(lhs.asIR, DFNet.Op.Connection, rhs.asIR)

object DFVarOps:
  extension [T <: DFTypeAny, A, C, I](dfVar: DFVal[T, Modifier[A, C, I]])
    def :=[R](rhs: Exact[R])(using
        varOnly: AssertGiven[
          A =:= Modifier.Assignable,
          "Cannot assign to an immutable dataflow value."
        ],
        tc: DFVal.TC[T, R],
        dfc: DFC
    ): Unit =
      dfVar.assign(tc(dfVar.dfType, rhs))

object DFPortOps:
  extension [T <: DFTypeAny, A, C, I](dfPort: DFVal[T, Modifier[A, C, I]])
    def <>[R](rhs: Exact[R])(using
        connectableOnly: AssertGiven[
          C =:= Modifier.Connectable,
          "The LHS of a connection must be a connectable dataflow value (var/port)."
        ],
        tc: DFVal.TC[T, R],
        dfc: DFC
    ): Unit =
      dfPort.connect(tc(dfPort.dfType, rhs))
