package dfhdl.core
import dfhdl.compiler.printing.Printer
import dfhdl.compiler.ir
import dfhdl.internals.*
import scala.annotation.targetName
import compiletime.*
import scala.quoted.*
import collection.mutable
import collection.immutable.ListMap
import DFOpaque.Abstract as DFOpaqueA

sealed trait Args
sealed trait NoArgs extends Args
sealed trait Args1[T1] extends Args
sealed trait Args2[T1, T2] extends Args
sealed trait Args3[T1, T2, T3] extends Args
sealed trait Args4[T1, T2, T3, T4] extends Args

final class DFType[+T <: ir.DFType, +A <: Args](val value: T | DFError) extends AnyVal:
  def ==(that: DFTypeAny)(using dfc: DFC): Boolean =
    import dfc.getSet
    this.asIR =~ that.asIR
  def !=(that: DFTypeAny)(using dfc: DFC): Boolean =
    import dfc.getSet
    !(this.asIR =~ that.asIR)
  override def toString: String = value.toString
type DFTypeAny = DFType[ir.DFType, Args]

object DFType:
  type Of[T <: Supported] <: DFTypeAny = T match
    case DFTypeAny => T <:! DFTypeAny
    case Long      => DFSInt[64]
    case Byte      => DFBits[8]
    case Boolean   => DFBool
    case DFOpaqueA => DFOpaque[T]
    case Product   => FromProduct[T]
    case Unit      => DFUnit

  type FromProduct[T <: Product] <: DFTypeAny = T match
    case DFEncoding      => DFEnum[T]
    case NonEmptyTuple   => DFTuple[Tuple.Map[T, JUSTVAL]]
    case DFStruct.Fields => DFStruct[T]

  type FromDFVal[T] <: DFTypeAny = T match
    case DFVal[t, ?] => t

  def of[T <: Supported](t: T)(using DFC): Of[T] = DFType(t).asInstanceOf[Of[T]]
  private[core] def apply(t: Any)(using DFC): DFTypeAny =
    t match
      case dfType: DFTypeAny         => dfType
      case tuple: NonEmptyTuple      => DFTuple(tuple)
      case tfe: DFOpaque.Frontend[?] => DFOpaque(tfe)
      case fields: DFStruct.Fields   => DFStruct(fields)
      case _: Byte.type              => DFBits(8)
      case _: Boolean.type           => DFBool
      case _: Int.type               => DFInt32
      case _: Long.type              => DFSInt(64)
      // TODO: need to add proper upper-bound if fixed in Scalac
      // see: https://contributors.scala-lang.org/t/missing-dedicated-class-for-enum-companions
      case enumCompanion: AnyRef => DFEnum(enumCompanion)
  private[core] def unapply(t: Any): Option[DFTypeAny] =
    t match
      case dfVal: DFValAny  => Some(dfVal.dfType)
      case DFTuple(dfType)  => Some(dfType)
      case DFStruct(dfType) => Some(dfType)
      case _                => None

  extension [T <: ir.DFType, A <: Args](dfType: DFType[T, A])
    def asIR: T = dfType.value match
      case dfTypeIR: T @unchecked => dfTypeIR
      case err: DFError.REG_DIN[?] if err.firstTime =>
        err.firstTime = false
        throw err
      case err: DFError => throw DFError.Derived(err)
    def codeString(using printer: Printer)(using DFC): String =
      printer.csDFType(asIR)
  extension (dfType: ir.DFType) def asFE[T <: DFTypeAny]: T = new DFType(dfType).asInstanceOf[T]
  extension (dfType: DFTypeAny) def asFE[T <: DFTypeAny]: T = dfType.asInstanceOf[T]
  transparent inline implicit def conv[T <: Supported](inline t: T)(implicit
      dfc: DFC,
      tc: TC[T]
  ): DFTypeAny = tc(t)
  export DFDecimal.Extensions.*
  export DFBoolOrBit.given
  export DFBits.given
  export DFDecimal.given
  export DFEnum.given
  export DFVector.given
  given [T <: DFTypeAny & Singleton](using ValueOf[T]): T = valueOf[T]

  given [T <: DFTypeAny]: CanEqual[T, T] = CanEqual.derived

  type Supported = DFTypeAny | NonEmptyTuple | DFStruct.Fields | DFEncoding | DFOpaqueA | Byte |
    Int | Long | Boolean | Object | Unit

  protected type NotGlobalCheck[S] = AssertGiven[
    util.NotGiven[S <:< DFC.Scope.Global],
    "Port/Variable declarations cannot be global"
  ]
  object Ops:
    extension [D <: Int & Singleton](cellDim: D)
      infix def <>[M <: ModifierAny](modifier: M)(using DFC): DFVector.ComposedModifier[D, M] =
        new DFVector.ComposedModifier[D, M](cellDim, modifier)
    extension [D <: IntP](cellDim: D)
      @targetName("composeMod")
      infix def <>[M <: ModifierAny](modifier: M)(using DFC): DFVector.ComposedModifier[D, M] =
        new DFVector.ComposedModifier[D, M](cellDim, modifier)
    extension [T <: Supported](t: T)
      infix def <>[A, C, I, P](modifier: Modifier[A, C, I, P])(using
          dfc: DFC,
          tc: DFType.TC[T],
          ck: DFC.Scope,
          dt: DomainType
      )(using NotGlobalCheck[ck.type]): DFVal[tc.Type, Modifier[A & ck.type & dt.type, C, I, P]] =
        trydf:
          if (modifier.value.isPort)
            dfc.owner.asIR match
              case _: ir.DFDomainOwner =>
              case _ =>
                throw new IllegalArgumentException(
                  "Ports can only be directly owned by a design, a domain or an interface."
                )
          DFVal.Dcl(tc(t), modifier.asInstanceOf[Modifier[A & ck.type & dt.type, C, I, P]])
    end extension
  end Ops

  trait TC[T]:
    type Type <: DFTypeAny
    def apply(t: T)(using DFC): Type
  trait TCLP:
    transparent inline given errorDMZ[T](using t: ShowType[T]): TC[T] =
      Error.call[
        (
            "Dataflow type cannot be constructed from the type `",
            t.Out,
            "`."
        )
      ]
  object TC extends TCLP:
    given ofDFType[T <: DFTypeAny]: TC[T] with
      type Type = T
      def apply(t: T)(using DFC): Type = t

    given ofBooleanCompanion: TC[Boolean.type] with
      type Type = DFBool
      def apply(t: Boolean.type)(using DFC): Type = DFBool

    given ofByteCompanion: TC[Byte.type] with
      type Type = DFBits[8]
      def apply(t: Byte.type)(using DFC): Type = DFBits(8)

    given ofIntCompanion: TC[Int.type] with
      type Type = DFInt32
      def apply(t: Int.type)(using DFC): Type = DFInt32

    given ofLongCompanion: TC[Long.type] with
      type Type = DFSInt[64]
      def apply(t: Long.type)(using DFC): Type = DFSInt(64)

    given ofOpaque[T <: DFTypeAny, TFE <: DFOpaque.Frontend[T]]: TC[TFE] with
      type Type = DFOpaque[TFE]
      def apply(t: TFE)(using DFC): Type = DFOpaque(t)

    transparent inline given ofProductCompanion[T <: AnyRef]: TC[T] = ${ productMacro[T] }
    def productMacro[T <: AnyRef](using Quotes, Type[T]): Expr[TC[T]] =
      import quotes.reflect.*
      val compObjTpe = TypeRepr.of[T]
      val compPrefix = compObjTpe match
        case TermRef(pre, _) => pre
        case _ =>
          report.errorAndAbort("Case class companion must be a term ref")
      val clsSym = compObjTpe.typeSymbol.companionClass
      if !clsSym.paramSymss.forall(_.headOption.forall(_.isTerm)) then
        report.errorAndAbort(
          "Case class with type parameters are not supported"
        )
      val clsTpe = compPrefix.select(clsSym)
      clsTpe.asType match
        case '[t & DFEncoding] =>
          '{
            new TC[T]:
              type Type = DFEnum[t & DFEncoding]
              def apply(t: T)(using DFC): Type = summonInline[DFEnum[t & DFEncoding]]
          }
        case '[t & DFStruct.Fields] =>
          '{
            new TC[T]:
              type Type = DFStruct[t & DFStruct.Fields]
              def apply(t: T)(using DFC): Type =
                summonInline[DFStruct[t & DFStruct.Fields]]
          }
        case '[t & DFOpaque.Abstract] =>
          '{
            new TC[T]:
              type Type = DFOpaque[t & DFOpaque.Abstract]
              def apply(t: T)(using DFC): Type =
                summonInline[DFOpaque[t & DFOpaque.Abstract]]
          }
        case _ =>
          val badTypeStr = clsTpe.show
          val msg =
            if (badTypeStr.endsWith("$package.<none>"))
              s"Type `$badTypeStr` is not a supported DFHDL type constructor.\nHint: Are you missing an argument in your DFHDL type constructor?"
            else
              s"Type `$badTypeStr` is not a supported product companion.\nHint: Did you forget to extends `Struct` or `Encode`?"
          '{ compiletime.error(${ Expr(msg) }) }
      end match
    end productMacro

    transparent inline given ofTuple[T <: NonEmptyTuple]: TC[T] = ${ ofTupleMacro[T] }
    def ofTupleMacro[T <: NonEmptyTuple](using Quotes, Type[T]): Expr[TC[T]] =
      import quotes.reflect.*
      val tTpe = TypeRepr.of[T]
      val args = tTpe.getTupleArgs
      val fun = defn.TupleClass(args.length).typeRef
      val tcTrees = args.map(t =>
        Implicits.search(TypeRepr.of[TC].appliedTo(t)) match
          case iss: ImplicitSearchSuccess =>
            iss.tree
          case isf: ImplicitSearchFailure =>
            report.errorAndAbort(isf.explanation)
      )
      val tcList = '{
        List(${ Varargs(tcTrees.map(_.asExpr)) }*).asInstanceOf[List[TC[Any]]]
      }
      val tpes = tcTrees
        .map(_.tpe.asTypeOf[Any] match
          case '[TC[t] { type Type = z }] => TypeRepr.of[z]
        )
        .map(t => TypeRepr.of[DFValOf].appliedTo(t))
      def applyExpr(t: Expr[T])(dfc: Expr[DFC]): Expr[List[DFTypeAny]] =
        '{
          val tList = $t.toList.asInstanceOf[List[Any]]
          $tcList.lazyZip(tList).map((tc, t) => tc(t)(using $dfc)).toList
        }
      val tplTpe = fun.appliedTo(tpes)
      val tplType = tplTpe.asTypeOf[NonEmptyTuple]
      '{
        new TC[T]:
          type Type = DFTuple[tplType.Underlying]
          def apply(t: T)(using dfc: DFC): Type =
            DFTuple[tplType.Underlying](${ applyExpr('t)('dfc) })
      }
    end ofTupleMacro
  end TC
end DFType

extension [T](t: T)(using tc: DFType.TC[T])
  @targetName("tcDFType")
  def dfType(using DFC): tc.Type = tc(t)

extension [T <: DFTypeAny, M <: ModifierAny](dfVal: DFVal[T, M])
  @targetName("dfValDFType")
  def dfType: T = dfVal.asIR.dfType.asFE[T]

extension (intParamRef: ir.IntParamRef)
  // currently unreachable type references are converted to literal integer parameters
  def dropUnreachableRef(allowDesignParamRefs: Boolean)(using dfc: DFC): ir.IntParamRef =
    import dfc.getSet
    intParamRef.getRef match
      case Some(ir.DFRef(dfVal)) =>
        // globals are always accessible
        if (dfVal.isGlobal) intParamRef
        // TODO: consider improving referencing so internal design types that propagate upwards
        // through ports conversion get special treatment (maybe referencing the original param at the owner design)
        // else if (allowDesignParamRefs)
        //   if (dfc.owner.asIR.getThisOrOwnerDesign == dfVal.getOwnerDesign) intParamRef
        //   else IntParam(dfVal.asConstOf[DFInt32]).ref
        else if (
          // only accessible values are within the same design
          allowDesignParamRefs && dfc.owner.asIR.getThisOrOwnerDesign == dfVal.getOwnerDesign
        )
          intParamRef
        // inline reference value
        else ir.IntParamRef(intParamRef.getInt)
      case _ => intParamRef
    end match
  end dropUnreachableRef
end extension

extension (dfType: ir.DFType)
  // drop unreachable type references for types that have type references.
  // this is meant to work during user code elaboration, since due to meta-programming
  // type referenced values may not always be directly accessible.
  // see `dropUnreachableRef` on ir.IntParamRef for more details.
  def dropUnreachableRefs(allowDesignParamRefs: Boolean)(using dfc: DFC): ir.DFType =
    // in compiler stages meta-programming we need to skip this, since there could be
    // temporarily unreachable references.
    if (dfc.inMetaProgramming) dfType
    else
      dfType match
        case ir.DFBits(widthParamRef) =>
          ir.DFBits(widthParamRef.dropUnreachableRef(allowDesignParamRefs))
        case ir.DFDecimal(signed, widthParamRef, fractionWidth, nativeType) =>
          ir.DFDecimal(
            signed,
            widthParamRef.dropUnreachableRef(allowDesignParamRefs),
            fractionWidth,
            nativeType
          )
        case ir.DFVector(cellType, cellDimParamRefs) =>
          ir.DFVector(
            cellType.dropUnreachableRefs(allowDesignParamRefs),
            cellDimParamRefs.map(_.dropUnreachableRef(allowDesignParamRefs))
          )
        // DFStruct/DFTuple/DFOpaque indeed contain other types, but at construction they are forced to have
        // no local references at all, so we can skip them. Other types that have no references at all
        // are trivially skipped.
        case _ => dfType
  end dropUnreachableRefs
  def dropUnreachableRefs(using DFC): ir.DFType = dropUnreachableRefs(true)
end extension
