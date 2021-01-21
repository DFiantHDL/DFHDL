package DFiant

import DFiant.DFAny.{UninitializedDcl, `Op==,!=`}
import DFiant.DFStruct.Fields
import DFiant.compiler.csprinter.CSPrinter
import internals._
import singleton.twoface.TwoFace
import compiler.printer.formatter._
import shapeless.LowPriority

import scala.collection.mutable
import language.dynamics
import scala.reflect.macros.whitebox

object DFStruct extends DFAny.Companion {
  abstract class Fields(implicit meta : Meta) extends DFAny.Token.Frontend with HasWidth { fields =>
    final private[DFStruct] val defaultFieldApplications = new Fields.FieldApplications
    final private[DFStruct] var currentFieldApplications = defaultFieldApplications
    final private[DFStruct] val all = mutable.ListBuffer.empty[DFStruct.Field[_ <: DFAny.Type]]
    final def getFieldList : List[DFStruct.Field[_]] = all.toList
    final private[DFStruct] lazy val __width : Int = all.map(_.dfType.width.getValue).sum
    final def ignoreFieldOverride() : Unit = currentFieldApplications.ignoredFieldOverride = true
    final def ignoreMissingFields() : Unit = currentFieldApplications.ignoredMissingFields = true
    protected trait __ExternalOnly
    implicit object __ExternalOnly1 extends __ExternalOnly
    protected implicit object __ExternalOnly2 extends __ExternalOnly
    protected sealed trait FIELD
    protected object FIELD extends FIELD
    final protected implicit class __AddField[Type <: DFAny.Type, T](t : T)(implicit tc : T => Type) {
      val dfType : Type = tc(t)
      def <> (FIELD : FIELD)(implicit meta : Meta) : DFStruct.Field[Type] = {
        val field = DFStruct.Field(dfType)
        all += field
        field
      }
    }
    final implicit class __InitOps[Type <: DFAny.Type](field : DFStruct.Field[Type])(implicit eo : __ExternalOnly) {
      def := [T](token : DFAny.Token.ToFit.Conv[Type, field.dfType.TToken]) : DFStruct.Field[Type] = {
        currentFieldApplications += field -> token(field.dfType)
        field
      }
    }
    final implicit class __InitOpsStruct[F <: Fields](structField : DFStruct.Field[Type[F]])(implicit eo : __ExternalOnly) {
      def := [T](fieldApplications : F => Unit)(implicit w : Fields.WidthOf[F]) : DFStruct.Field[Type[F]] = {
        val token = structField.dfType.fields.tokenGen(fieldApplications)
        currentFieldApplications += structField -> token
        structField
      }
      def := [T](token : TokenF[F]) : DFStruct.Field[Type[F]] = {
        currentFieldApplications += structField -> token
        structField
      }
    }
    private[DFiant] final def name(implicit getSet: MemberGetSet) : String = getSet.getGlobalTag[DFMember.NameTag](this) match {
      case Some(DFMember.NameTag(taggedName)) => taggedName
      case _ => meta.name
    }
    final override def equals(obj : Any) : Boolean = obj match {
      case fields : Fields => (fields.all lazyZip all).forall {
        case (value1, value2) => value1 == value2
      }
      case _ => false
    }
  }
  object Fields {
    implicit def dfTypeTC[F <: Fields](implicit w : WidthOf[F]) : F => Type[F] =
      f => DFStruct.Type(f)

    trait WidthOf[F <: Fields] {
      type Out
    }
    object WidthOf {
      implicit def ev[F <: Fields] : WidthOf[F] = macro evMacro[F]
      final def dud[F <: Fields] = new WidthOf[F]{type Out = Int}
      def evMacro[F <: Fields : c.WeakTypeTag](c : whitebox.Context) : c.Tree = {
        import c.universe._
        val tpe = weakTypeOf[F]
        val fieldTpe = typeOf[Field[_]]
        val fieldsSym = symbolOf[Fields]
        val publicFieldTrees =
          if (tpe <:< typeOf[DFTuple.Fields]) tpe.typeArgs.map(i => q"""valueOf[$i#Width]""")
          else if (tpe <:< typeOf[DFOpaque.Of[_,_]]) {
            val bt = tpe.baseType(symbolOf[DFOpaque.Of[_,_]]).typeArgs.head
            List(q"""valueOf[$bt#Width]""")
          }
          else {
            tpe.baseClasses
              .takeWhile(bc => bc != fieldsSym)
              .flatMap(_.info.decls)
              .filter(d => d.info.finalResultType <:< fieldTpe && d.isPublic)
              .map(p => q"valueOf[$p.Width]")
          }
        val widthOption = publicFieldTrees.map(tree => c.typecheck(tree,silent = true).tpe.dealias).map {
          case ConstantType(Constant(width : Int)) => Some(width)
          case _ => None
        }.reduce[Option[Int]] {
          case (Some(left), Some(right)) => Some(left + right)
          case _ => None
        }

        val widthTpe = widthOption match {
          case Some(value) => c.internal.constantType(Constant(value))
          case None => typeOf[Int]
        }
        q"new DFiant.DFStruct.Fields.WidthOf[$tpe]{type Out = $widthTpe}"
      }
    }
    class FieldApplications {
      val fieldTokens = mutable.Map.empty[Field[_], DFAny.Token]
      def copy() : FieldApplications = {
        val newFA = new FieldApplications
        newFA.fieldTokens ++= fieldTokens
        newFA
      }
      private[DFStruct] var ignoredFieldOverride : Boolean = false
      private[DFStruct] var ignoredMissingFields : Boolean = false
      def += (fieldApplication : (Field[_], DFAny.Token)) : Unit = {
        if (fieldTokens.contains(fieldApplication._1) && !ignoredFieldOverride)
          throw new IllegalArgumentException(s"\nOverriding an existing field value `${fieldApplication._1.fieldName}`. Enable this by calling `.ignoreFieldOverride()`.")
        fieldTokens += fieldApplication
      }
    }
    implicit class TokenGen[F <: Fields](fields : F) {
      def tokenGen(applyBlock : F => Unit)(implicit w : WidthOf[F]) : TokenF[F] = {
        fields.currentFieldApplications = fields.defaultFieldApplications.copy()
        applyBlock(fields)
        if (fields.currentFieldApplications.fieldTokens.size != fields.all.size && !fields.currentFieldApplications.ignoredMissingFields) {
          val missingFields = fields.all.toSet -- fields.currentFieldApplications.fieldTokens.keys
          val missingFieldNames = missingFields.map(_.fieldName).mkString(", ")
          throw new IllegalArgumentException(
            s"""
               |Some token fields are not set. Enable this by calling `.ignoreMissingFields()` or set the following fields:
               |$missingFieldNames""".stripMargin
          )
        }
        Token(fields, fields.currentFieldApplications.fieldTokens.toMap).asInstanceOf[TokenF[F]]
      }
    }
  }

  final case class Field[Type <: DFAny.Type](dfType : Type)(
    implicit meta : Meta
  ) {
    type Width = dfType.Width
    private[DFiant] val fieldName : String = meta.name

    override def equals(obj : Any) : Boolean = obj match {
      case field : Field[_] => field.dfType == dfType && field.fieldName == fieldName
      case _ => false
    }
  }
  class DFFields[F <: Fields, Mod <: DFAny.Modifier](protected val struct : DFAny.Value[Type[F], Mod])
  object DFFields {
    def applyMacro[F <: Fields : c.WeakTypeTag, Mod <: DFAny.Modifier : c.WeakTypeTag](c : whitebox.Context) : c.Tree = {
      import c.universe._
      val tpe = weakTypeOf[F]
      val fieldTpe = typeOf[Field[_]]
      val fieldsSym = symbolOf[Fields]
      val q"$ignore($structTree)" = c.prefix.tree
      val publicFields = tpe.baseClasses
        .takeWhile(bc => bc != fieldsSym)
        .flatMap(_.info.decls)
        .filter(d => d.info.finalResultType <:< fieldTpe && d.isPublic)
      val defFields = publicFields.map{d =>
        val fieldAccessTree = q"struct.dfType.fields.${d.name.toTermName}"
        val dfTypeTree = q"${fieldAccessTree}.dfType"
        val selectorTree = q"DFiant.DFStruct.Selector($dfTypeTree, struct, ${d.name.toString})(ctx)"
        q"def ${d.name.toTermName}(implicit ctx : DFiant.DFAny.Context) = $selectorTree"
      }
      val genTree =
        q"""
            new DFiant.DFStruct.DFFields($structTree){
              ..$defFields
            }
         """
//      println(genTree)
      genTree
    }
  }
  final case class Selector(
    dfType : DFAny.Type, modifier : DFAny.Modifier, structRef : Selector.Ref, fieldName : String, ownerRef : DFOwner.Ref, tags : DFMember.Tags
  ) extends DFAny.Member {
    protected[DFiant] def =~(that : DFMember)(implicit getSet : MemberGetSet) : Boolean = that match {
      case Selector(dfType, modifier, _, fieldName, _, tags) =>
        this.dfType == dfType && this.modifier == modifier && this.fieldName == fieldName && this.tags =~ tags
      case _ => false
    }
    def codeString(implicit printer: CSPrinter): String = {
      import printer.config._
      val fieldCS = structRef.get.dfType match {
        case DFOpaque.Type(_) => s"$DF${fieldName}"
        case _ => fieldName
      }
      s"${structRef.refCodeString.applyBrackets()}.$fieldCS"
    }

    override def refCodeString(implicit printer : CSPrinter, owner : DFOwner) : String = codeString

    def setTags(tagsFunc : DFMember.Tags => DFMember.Tags)(implicit getSet : MemberGetSet) : DFMember =
      getSet.set(this)(m => m.copy(tags = tagsFunc(m.tags)))
  }
  object Selector {
    type Ref = DFMember.OwnedRef.Of[Ref.Type, DFAny.Member]
    object Ref {
      trait Type extends DFAny.Ref.ConsumeFrom.Type
      implicit val ev : Type = new Type {}
    }
    def apply[F <: Fields, Type <: DFAny.Type, Mod <: DFAny.Modifier](
      dfType : Type, struct : DFAny.Value[DFStruct.Type[F], Mod], fieldName : String
    )(
      implicit ctx: DFAny.Context
    ): DFAny.Value[Type, Mod] = {
      implicit lazy val ret : DFAny.Value[Type, Mod] with DFMember.RefOwner =
        ctx.db.addMember(
          Selector(dfType, struct.member.modifier, struct.member, fieldName, ctx.owner, ctx.meta.anonymize)
        ).asRefOwner[Type, Mod]
      ret
    }
  }

  final case class Type[F <: Fields](fields : F)(implicit val __w : Fields.WidthOf[F]) extends DFAny.Type {
    type Width = __w.Out
    type TToken = Token
    type TPattern = Nothing
    type TPatternAble[+R] = Nothing
    type TPatternBuilder[LType <: DFAny.Type] = Nothing
    val width : TwoFace.Int[Width] = TwoFace.Int.create[Width](fields.__width)
    def getBubbleToken : TToken = Token.bubble(fields)
    def assignCheck(from : DFAny.Member)(implicit ctx : DFAny.Context) : Unit = from match {
      case DFStruct(fields) if this.fields == fields =>
    }
    def codeString(implicit printer : CSPrinter) : String = {
      import printer.config._
      fields match {
        case _ : DFTuple.Fields =>
          fields.all.view.map(f => f.dfType.codeString).mkString("(",", ",")")
        case opaqueFields : DFOpaque.Fields =>
          opaqueFields.codeString
        case _ => fields.name
      }
    }
    def getTokenFromBits(fromToken : DFBits.Token) : DFAny.Token = {
      assert(fromToken.width == width.getValue)
      var relBitHigh = fromToken.width-1
      val value : Map[Field[_], DFAny.Token] = fields.all.map { f =>
        val relWidth = f.dfType.width
        val relBitLow = relBitHigh - relWidth + 1
        relBitHigh = relBitLow - 1
        f -> f.dfType.getTokenFromBits(fromToken.bitsWL(relWidth, relBitLow))
      }.toMap
      Token(fields, value)
    }

    override def equals(obj : Any) : Boolean = obj match {
      case Type(fields) => fields == this.fields
      case _ => false
    }
  }

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Public Constructors
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  def apply[F <: Fields](implicit v : ValueOf[F], w : Fields.WidthOf[F], di : DummyImplicit) : Type[F] = Type(valueOf[F])
  def apply[F <: Fields](fields : F)(implicit w : Fields.WidthOf[F]) : Type[F] =
    Type(fields)
  def unapply(arg: DFAny.Member): Option[Fields] = arg.dfType match {
    case Type(fields) => Some(fields)
    case _ => None
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Frontend
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  object Frontend {
    trait Inherited extends Op.Frontend.Inherited with Token.Frontend.Inherited
    trait Imported extends Op.Frontend.Imported with Token.Frontend.Imported
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Token
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  type TokenF[F <: Fields] = DFAny.TokenT[Token, Type[F]]
  final case class Token(fields : Fields, value : Map[Field[_], DFAny.Token]) extends DFAny.Token { left =>
    val dfType : DFAny.Type = Type(fields)(Fields.WidthOf.dud[Fields])
    val width : Int = dfType.width
    lazy val (valueBits, bubbleMask) : (BitVector, BitVector) = {
      val token = fields.all
        .map(f => value.get(f).map(_.bits)
        .getOrElse(DFBits.Token.bubble(f.dfType.width)))
        .reduce(_ ++ _)
      (token.valueBits, token.bubbleMask)
    }
    def == (right : DFAny.Token)(implicit bb : Bubble.Behaviour) : DFBool.Token = right match {
      case right : Token =>
        DFBool.Token(logical = true, left.value == right.value)
      case _ => ???
    }
    def codeString(implicit printer : CSPrinter) : String = {
      import printer.config._
      fields match {
        case _ : DFTuple.Fields =>
          fields.all.view
            .map(f =>  s"${value.getOrElse(f, f.dfType.getBubbleToken).codeString}")
            .mkString("(",", ", ")")
        case opaqueFields : DFOpaque.Fields =>
          s"${opaqueFields.codeString}(${value(fields.all.head).codeString})"
        case _ =>
          val fieldApplications =
            fields.all.view
              .map(f =>  s"${f.fieldName} := ${value.getOrElse(f, f.dfType.getBubbleToken).codeString}")
              .mkString("; ")
          s"{t => $DF import t._; ${fieldApplications}}"
      }
    }
  }
  object Token {
    def bubble(fields: Fields) : Token = Token(fields, Map())

    type Summon[F <: Fields, V] = DFAny.Token.Exact.Summon.SAM[Type[F], V, TokenF[F]]
    sealed trait Frontend {
      protected implicit def __DFStructToken[F <: Fields]
      : Summon[F, TokenF[F]] = (from, value) => value
    }
    object Frontend {
      trait Inherited extends Frontend {
        final override protected implicit def __DFStructToken[F <: Fields] : Summon[F, TokenF[F]] = super.__DFStructToken
      }
      trait Imported extends Frontend {
        final override implicit def __DFStructToken[F <: Fields] : Summon[F, TokenF[F]] = super.__DFStructToken
      }
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Match Pattern
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
//  class Pattern(set : Set[DFEnum.Entries.Entry]) extends DFAny.Pattern.OfSet[Type[DFEnum.Entries], DFEnum.Entries.Entry, Pattern](set) {
//    protected def matchCond(matchVal: DFAny.Of[Type[DFEnum.Entries]], value : DFEnum.Entries.Entry)(
//      implicit ctx: DFAny.Context
//    ): DFBool = {
//      import DFDesign.Frontend._
//      matchVal === value.asInstanceOf[DFEnum.Entries#Entry]
//    }
//  }
  object Pattern extends PatternCO {
//    trait Able[+R] extends DFAny.Pattern.Able[R]
//    object Able {
//      implicit class DFEnumPattern[E <: DFEnum.Entries](val right : E#Entry) extends Able[E#Entry]
//    }
//    trait Builder[LType <: DFAny.Type] extends DFAny.Pattern.Builder[LType, Able]
//    object Builder {
//      implicit def ev[E <: DFEnum.Entries] : Builder[Type[E]] = new Builder[Type[E]] {
//        def apply[R](left: Type[E], right: Seq[Able[R]]): Pattern = {
//          new Pattern(right.map(e => e.right.asInstanceOf[E#Entry]).toSet)
//        }
//      }
//    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Op
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  object Op {
    class Able[L](val value : L) extends DFAny.Op.Able[L]
    class AbleOps[L](value : L) extends Able[L](value) {
      val left = value
      final def === [F <: Fields](right : DFStruct[F])(implicit op: DFAny.`Op==`.Builder[L, DFStruct[F]]) = op(left, right)
      final def =!= [F <: Fields](right : DFStruct[F])(implicit op: DFAny.`Op!=`.Builder[L, DFStruct[F]]) = op(left, right)
    }
    sealed trait Frontend {
      protected sealed class __DFStructFromToken[F <: Fields](left : TokenF[F]) extends AbleOps[TokenF[F]](left)
      protected implicit def __DFStructFromToken[F <: Fields](left: TokenF[F]): __DFStructFromToken[F] = new __DFStructFromToken(left)
      protected implicit def __ofDFStruct[F <: Fields](left : DFStruct[F]) : Able[DFStruct[F]] = new Able(left)
      protected implicit class __DFStructOps[F <: Fields](val left : DFStruct[F]){
        def === [R](right : Exact[R])(implicit op: DFAny.`Op==`.Builder[DFStruct[F], R]) = op(left, right)
        def =!= [R](right : Exact[R])(implicit op: DFAny.`Op!=`.Builder[DFStruct[F], R]) = op(left, right)
      }
      protected implicit def __DFStruct_eq_Capable[F <: Fields]
      : DFAny.`Op==,!=`.Capable[Type[F], Type[F]] =
        (left, right) => assert(left.fields == right.fields)

      protected implicit class __InitializableOps[F <: Fields](
        val left : DFAny.UninitializedDcl[Type[F]]
      )(implicit lp : shapeless.LowPriority) {
        def init(fieldApplications : (F => Unit)*)(
          implicit ctx : DFAny.Context, w : Fields.WidthOf[F]
        ) : DFAny.DclOf[Type[F]] = {
          val tokenSeq = fieldApplications.map(fa => left.dfType.fields.tokenGen(fa))
          left.forcedInit(tokenSeq)
        }
        def init(tokenSeq : TokenF[F]*)(
          implicit ctx : DFAny.Context, di : DummyImplicit
        ) : DFAny.DclOf[Type[F]] = {
          left.forcedInit(tokenSeq)
        }
      }
    }
    object Frontend {
      trait Inherited extends Frontend {
        final override protected implicit def __DFStructFromToken[F <: Fields](left : TokenF[F]) : __DFStructFromToken[F] = super.__DFStructFromToken(left)
        final override protected implicit def __ofDFStruct[F <: Fields](left : DFStruct[F]) : Able[DFStruct[F]] = super.__ofDFStruct(left)
        final override protected implicit def __DFStructOps[F <: Fields](left : DFStruct[F]) : __DFStructOps[F] = super.__DFStructOps(left)
        final override protected implicit def __InitializableOps[F <: Fields](left : UninitializedDcl[Type[F]])(implicit lp : LowPriority) : __InitializableOps[F] = super.__InitializableOps(left)
        final override protected implicit def __DFStruct_eq_Capable[F <: Fields] : `Op==,!=`.Capable[Type[F], Type[F]] = super.__DFStruct_eq_Capable
      }
      trait Imported extends Frontend {
        final override implicit def __DFStructFromToken[F <: Fields](left : TokenF[F]) : __DFStructFromToken[F] = super.__DFStructFromToken(left)
        final override implicit def __ofDFStruct[F <: Fields](left : DFStruct[F]) : Able[DFStruct[F]] = super.__ofDFStruct(left)
        final override implicit def __DFStructOps[F <: Fields](left : DFStruct[F]) : __DFStructOps[F] = super.__DFStructOps(left)
        final override implicit def __InitializableOps[F <: Fields](left : UninitializedDcl[Type[F]])(implicit lp : LowPriority) : __InitializableOps[F] = super.__InitializableOps(left)
        final override implicit def __DFStruct_eq_Capable[F <: Fields] : `Op==,!=`.Capable[Type[F], Type[F]] = super.__DFStruct_eq_Capable
      }
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
}
