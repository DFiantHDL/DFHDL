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
type DFVarAny = DFVal[DFTypeAny, Modifier.Mutable]
type DFDclAny = DFVal[DFTypeAny, Modifier.Dcl]
type DFValOf[+T <: DFTypeAny] = DFVal[T, ModifierAny]
type DFConstOf[+T <: DFTypeAny] = DFVal[T, Modifier.CONST]
type DFValTP[+T <: DFTypeAny, +P] = DFVal[T, Modifier[Any, Any, Any, P]]
type DFVarOf[+T <: DFTypeAny] = DFVal[T, Modifier.Mutable]

sealed trait TOKEN
infix type <>[T <: DFType.Supported, M] = T match
  case Int => // Int can be a constant literal or just "Int" representing SInt[32]
    IsConst[T] match
      case true  => DFVector.ComposedModifier[T, M]
      case false => DFSInt[32] <> M
  case _ =>
    M match
      case DFRET => DFC ?=> DFValOf[DFType.Of[T]]
      case VAL   => DFValOf[DFType.Of[T]]
      case CONST => DFConstOf[DFType.Of[T]]
      case TOKEN => DFToken[DFType.Of[T]]

infix type X[T <: DFType.Supported, M] = M match
  case DFVector.ComposedModifier[d, m] => <>[DFVector[DFType.Of[T], Tuple1[d]], m]
  case Int & Singleton                 => DFVector[DFType.Of[T], Tuple1[M]]
type JUSTVAL[T <: DFType.Supported] = <>[T, VAL]

extension (dfVal: ir.DFVal)
  inline def asVal[T <: DFTypeAny, M <: ModifierAny]: DFVal[T, M] =
    DFVal[T, M](dfVal)
  inline def asValOf[T <: DFTypeAny]: DFValOf[T] =
    DFVal[T, ModifierAny](dfVal)
  inline def asValTP[T <: DFTypeAny, P]: DFValTP[T, P] =
    DFVal[T, Modifier[Any, Any, Any, P]](dfVal)
  inline def asValAny: DFValAny =
    DFVal[DFTypeAny, ModifierAny](dfVal)
  inline def asVarOf[T <: DFTypeAny]: DFVarOf[T] =
    DFVal[T, Modifier.Mutable](dfVal)
  inline def asVarAny: DFVarAny =
    DFVal[DFTypeAny, Modifier.Mutable](dfVal)
  inline def asDclAny: DFDclAny =
    DFVal[DFTypeAny, Modifier.Dcl](dfVal)
  inline def asConstOf[T <: DFTypeAny]: DFConstOf[T] =
    DFVal[T, Modifier.CONST](dfVal)
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
  '{
    import DFStruct.apply
    given bitsNoType: DFBits[Int] = DFNothing.asInstanceOf[DFBits[Int]]
    given uintNoType: DFUInt[Int] = DFNothing.asInstanceOf[DFUInt[Int]]
    given sintNoType: DFSInt[Int] = DFNothing.asInstanceOf[DFSInt[Int]]
    val conv = compiletime.summonInline[DFVal.Conv[T, P, fromExactType.Underlying]]
    val dfc = compiletime.summonInline[DFC]
    val dfType = compiletime.summonInline[T]
    trydf {
      conv(dfType, $fromExactExpr)(using dfc)
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
      W <: Int,
      P <: Boolean,
      R <: DFValAny | DFTokenAny | SameElementsVector[?] | NonEmptyTuple | Bubble
  ](
      inline from: R
  ): DFValTP[DFBits[W], ISCONST[P]] = ${
    DFValConversionMacro[DFBits[W], ISCONST[P], R]('from)
  }
  // TODO: candidate should be fixed to cause UInt[?]->SInt[Int] conversion
  implicit transparent inline def DFXIntValConversion[
      S <: Boolean,
      W <: Int,
      P <: Boolean,
      R <: DFValAny | DFTokenAny | Int | Bubble
  ](
      inline from: R
  ): DFValTP[DFXInt[S, W], ISCONST[P]] = ${
    DFValConversionMacro[DFXInt[S, W], ISCONST[P], R]('from)
  }
  implicit transparent inline def DFOpaqueValConversion[
      TFE <: DFOpaque.Abstract,
      P <: Boolean,
      R <: DFValAny | DFTokenAny | Bubble
  ](
      inline from: R
  ): DFValTP[DFOpaque[TFE], ISCONST[P]] = ${
    DFValConversionMacro[DFOpaque[TFE], ISCONST[P], R]('from)
  }
  implicit transparent inline def DFStructValConversion[
      F <: DFStruct.Fields,
      P <: Boolean,
      R <: DFValAny | DFTokenAny | DFStruct.Fields | Bubble
  ](
      inline from: R
  ): DFValTP[DFStruct[F], ISCONST[P]] = ${
    DFValConversionMacro[DFStruct[F], ISCONST[P], R]('from)
  }
  implicit transparent inline def DFTupleValConversion[
      T <: NonEmptyTuple,
      P <: Boolean,
      R <: DFValAny | DFTokenAny | NonEmptyTuple | Bubble
  ](
      inline from: R
  ): DFValTP[DFTuple[T], ISCONST[P]] = ${
    DFValConversionMacro[DFTuple[T], ISCONST[P], R]('from)
  }
  given DFBitValConversion[R <: BitOrBool](using
      tc: DFVal.TC[DFBit, R],
      dfc: DFC
  ): Conversion[R, DFValOf[DFBit]] = from => tc(DFBit, from).asInstanceOf[DFValOf[DFBit]]
  given DFBoolValConversion[R <: BitOrBool](using
      tc: DFVal.TC[DFBool, R],
      dfc: DFC
  ): Conversion[R, DFValOf[DFBool]] = from => tc(DFBool, from).asInstanceOf[DFValOf[DFBool]]
  given DFUnitValConversion[R <: DFValAny | Unit | NonEmptyTuple | Bubble](using
      dfc: DFC
  ): Conversion[R, DFValOf[DFUnit]] = from => DFUnitVal().asInstanceOf[DFValOf[DFUnit]]
  given ConstToNonConstAccept[T <: DFTypeAny, P]: Conversion[DFValTP[T, P], DFValTP[T, NOTCONST]] =
    from => from.asValTP[T, NOTCONST]
  // implicit transparent inline def NonConstToConstReject[T <: DFTypeAny, P](
  //     from: DFValTP[T, P]
  // )(using util.NotGiven[P =:= CONST]): DFValTP[T, CONST] =
  //   compiletime.error(DFVal.ConstOnlyMsg)
end DFValLP
object DFVal extends DFValLP:
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
      c($dfVal, $exactExpr)(using
        dfc,
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

  given __refined_dfVal[T <: FieldsOrTuple, A, I, P](using
      r: DFStruct.Val.Refiner[T, A, I, P]
  ): Conversion[DFVal[DFStruct[T], Modifier[A, Any, I, P]], r.Out] = _.asInstanceOf[r.Out]

  trait InitCheck[I]
  given [I](using
      initializableOnly: AssertGiven[
        I =:= Modifier.Initializable,
        "Can only initialize a DFHDL port or variable that are not already initialized."
      ]
  ): InitCheck[I] with {}

  extension [T <: DFTypeAny, M <: ModifierAny](dfVal: DFVal[T, M])
    infix def tag[CT <: ir.DFTag: ClassTag](customTag: CT)(using
        dfc: DFC
    ): DFVal[T, M] =
      import dfc.getSet
      dfVal.asIR
        .setTags(_.tag(customTag))
        .setMeta(m => if (m.isAnonymous && !dfc.getMeta.isAnonymous) dfc.getMeta else m)
        .asVal[T, M]
    infix def tag[CT <: ir.DFTag: ClassTag](condCustomTag: Conditional[CT])(using
        dfc: DFC
    ): DFVal[T, M] = if (condCustomTag.isActive) dfVal.tag(condCustomTag.getArg) else dfVal
    def hasTag[CT <: ir.DFTag: ClassTag](using dfc: DFC): Boolean =
      import dfc.getSet
      dfVal.asIR.tags.hasTagOf[CT]
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
  end extension

  case object ExtendTag extends ir.DFTagOf[ir.DFVal]
  type ExtendTag = ExtendTag.type
  case object TruncateTag extends ir.DFTagOf[ir.DFVal]
  type TruncateTag = TruncateTag.type

  extension [T <: DFTypeAny, A, C, I, P, R](dfVal: DFVal[T, Modifier[A, C, I, P]])
    private[core] def initForced(tokens: List[ir.DFTokenAny])(using
        dfc: DFC
    ): DFVal[T, Modifier[A, C, Modifier.Initialized, P]] =
      import dfc.getSet
      require(
        dfVal.asIR.isAnonymous,
        s"Cannot initialize a named value ${dfVal.asIR.getFullName}. Initialization is only supported at the declaration of the value."
      )
      val updateDcl =
        dfVal.asIR.asInstanceOf[ir.DFVal.Dcl].copy(externalInit = Some(tokens), meta = dfc.getMeta)
      dfc.getSet.replace(dfVal.asIR)(updateDcl).asVal[T, Modifier[A, C, Modifier.Initialized, P]]

    infix def init(
        tokenValues: DFToken.Value[T]*
    )(using DFC, InitCheck[I]): DFVal[T, Modifier[A, C, Modifier.Initialized, P]] = trydf {
      val tvList = tokenValues.view.filter(_.enable).map(tv => tv(dfVal.dfType).asIR).toList
      if (tvList.isEmpty) dfVal.asVal[T, Modifier[A, C, Modifier.Initialized, P]]
      else initForced(tvList)
    }
  end extension
  extension [T <: NonEmptyTuple, A, C, I, P](dfVal: DFVal[DFTuple[T], Modifier[A, C, I, P]])
    infix def init(
        tokenValues: DFToken.TupleValues[T]
    )(using DFC, InitCheck[I]): DFVal[DFTuple[T], Modifier[A, C, Modifier.Initialized, P]] =
      trydf {
        if (tokenValues.enable) dfVal.initForced(tokenValues(dfVal.dfType).map(_.asIR))
        else dfVal.asVal[DFTuple[T], Modifier[A, C, Modifier.Initialized, P]]
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
    def apply[T <: DFTypeAny](token: DFToken[T], named: Boolean = false)(using
        DFC
    ): DFConstOf[T] =
      val meta = if (named) dfc.getMeta else dfc.getMeta.anonymize
      ir.DFVal
        .Const(token.asIR, dfc.owner.ref, meta, ir.DFTags.empty)
        .addMember
        .asConstOf[T]

  object Open:
    def apply[T <: DFTypeAny](dfType: T)(using DFC): DFValOf[T] =
      ir.DFVal
        .Open(dfType.asIR, dfc.owner.ref)
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
          None,
          dfc.owner.ref,
          dfc.getMeta,
          ir.DFTags.empty
        )
        .addMember
        .asVal[T, M]
  end Dcl

  object Func:
    export ir.DFVal.Func.Op
    def apply[T <: DFTypeAny, P](
        dfType: T,
        op: FuncOp,
        args: List[DFValTP[?, P]]
    )(using DFC): DFValTP[T, P] = apply(dfType, op, args.map(_.asIR))
    @targetName("applyFromIR")
    def apply[T <: DFTypeAny, P](
        dfType: T,
        op: FuncOp,
        args: List[ir.DFVal]
    )(using DFC): DFValTP[T, P] =
      lazy val func: ir.DFVal = ir.DFVal.Func(
        dfType.asIR,
        op,
        args.map(_.refTW(func)),
        dfc.owner.ref,
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
    type OutP
    type Out = DFValTP[T, OutP]
    type Ctx = DFC
    @metaContextIgnore
    final def apply(dfType: T, value: R)(using DFC): Out = trydf:
      conv(dfType, value)

  trait TCLP:
    // Accept OPEN in compile-time, but throw exception where it should not be used
    given fromOPEN[T <: DFTypeAny]: TC[T, __OPEN.type] with
      def conv(dfType: T, value: __OPEN.type)(using Ctx): Out =
        throw new IllegalArgumentException("OPEN cannot be used here")
    // Accept any bubble value
    given fromBubble[T <: DFTypeAny, V <: Bubble](using
        tokenTC: DFToken.TC[T, V]
    ): TC[T, V] with
      type OutP = CONST
      def conv(dfType: T, value: V)(using Ctx): Out =
        Const(tokenTC(dfType, value), named = true)
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
      def conv(dfType: T, value: V)(using Ctx): DFValTP[T, P] =
        given Printer = DefaultPrinter
        given MemberGetSet = dfc.getSet
        require(
          dfType == value.dfType,
          s"Unsupported value of type `${value.dfType.codeString}` for DFHDL receiver type `${dfType.codeString}`."
        )
        value
    given sameValAndTokenType[T <: DFTypeAny, V <: T <> TOKEN]: TC[T, V] with
      type OutP = CONST
      def conv(dfType: T, value: V)(using Ctx): Out =
        given Printer = DefaultPrinter
        given MemberGetSet = dfc.getSet
        require(
          dfType == value.dfType,
          s"Unsupported value of type `${value.dfType.codeString}` for DFHDL receiver type `${dfType.codeString}`."
        )
        DFVal.Const(value, named = true)
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

  trait Conv[T <: DFTypeAny, P, R]:
    def apply(dfType: T, value: R)(using DFC): DFValTP[T, P]
  object Conv:
    transparent inline given [T <: DFTypeAny, P, R](using tc: TC[T, R]): Conv[T, P, R] =
      val constCheck = compiletime.summonInline[AssertGiven[
        util.NotGiven[P =:= CONST] | (tc.OutP =:= CONST),
        "Applied argument is not a constant."
      ]]
      new Conv[T, P, R]:
        def apply(dfType: T, value: R)(using DFC): DFValTP[T, P] =
          tc(dfType, value).asValTP[T, P]

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
    ): DFValOf[DFBool] = trydf:
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
        DFVal.Const(arg, named = true)
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
  final val ConstOnlyMsg = "Applied argument is not a constant."
  trait ConstOnly[P]
  given [P](using
      AssertGiven[P =:= CONST, "Applied argument is not a constant."]
  ): ConstOnly[P] with {}

  trait ConvConstCheck[LP, RP]
  given [LP, RP](using
      AssertGiven[
        util.NotGiven[LP =:= CONST] | (RP =:= CONST),
        ConstOnlyMsg.type
      ]
  ): ConvConstCheck[LP, RP] with {}

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
    extension [T <: DFTypeAny, A, C, I, S <: Int, V](dfVal: DFVal[T, Modifier[A, C, I, Any]])
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

    extension [T <: DFTypeAny, A, C, I, P](dfVal: DFVal[T, Modifier[A, C, I, P]])
      def bits(using w: Width[T])(using DFC): DFValTP[DFBits[w.Out], P] = trydf {
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
  extension [T <: DFTypeAny, A](dfVar: DFVal[T, Modifier[A, Any, Any, Any]])
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
  extension [T <: DFTypeAny, C](dfPort: DFVal[T, Modifier[Any, C, Any, Any]])
    def <>[R](rhs: Exact[R])(using
        connectableOnly: AssertGiven[
          C <:< Modifier.Connectable,
          "The LHS of a connection must be a connectable DFHDL value (var/port)."
        ],
        tc: DFVal.TC[T, R],
        dfc: DFC
    ): Unit =
      if (rhs.value equals __OPEN) dfPort.connect(DFVal.Open(dfPort.dfType))
      else trydf { dfPort.connect(tc(dfPort.dfType, rhs)) }
end DFPortOps
