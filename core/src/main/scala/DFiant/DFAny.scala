package DFiant
import singleton.ops._
import singleton.twoface._
import DFiant.internals._

import language.dynamics
import scala.annotation.implicitNotFound
import DFiant.DFDesign.DB
import compiler.printer.formatter._
import compiler.csprinter._
import singleton.ops.impl.HasOut

import scala.reflect.macros.{TypecheckException, whitebox}

/**
  * Any Dataflow Variable
  *
  * This is the frontend trait. It contains a `member` reference to the actual member being placed in the IR.
  */
sealed trait DFAny extends HasWidth {
  type TType <: DFAny.Type
  type TMod <: DFAny.Modifier
  val dfType : TType
  val modifier : TMod
  def member : DFAny.Member
  type Width = dfType.Width
  type TToken = dfType.TToken
  type Arg[R] = DFAny.`Op:=,<>`.Builder[TType, R]
  final lazy val width = dfType.width
  final protected val left : this.type = this
  protected type AsVal = DFAny.Value[TType, DFAny.Modifier.Val]
  protected type AsVar = DFAny.VarOf[TType]
  protected[DFiant] type AsType[T <: DFAny.Type] = DFAny.Value[T, TMod]
  protected type This = DFAny.Of[TType]
}

object DFAny {
  /**
    * The dataflow variable reference in the IR.
    */
  trait Member extends DFMember with Product with Serializable {
    val dfType : DFAny.Type
    val modifier : DFAny.Modifier
    final lazy val width : Int = dfType.width.getValue
    def codeString(implicit printer: CSPrinter) : String
    def refCodeString(implicit printer: CSPrinter, owner : DFOwner) : String = {
      val callOwner: DFBlock = owner match {
        case b : DFBlock => b
        case o => o.getOwnerBlock
      }
      if (isAnonymous) codeString
      else getRelativeName(callOwner, printer.getSet)
    }
    final def asValOf[Type <: DFAny.Type] : DFAny.Of[Type] =
      DFAny.Value[Type, Modifier.Val](this)
    final def asVal : DFAny.Of[DFAny.Type] = asValOf[DFAny.Type]
    final def asVarOf[Type <: DFAny.Type] : DFAny.VarOf[Type] =
      DFAny.Value[Type, Modifier.Assignable](this)
    final def asVar : DFAny.VarOf[DFAny.Type] = asVarOf[DFAny.Type]
    final def asValModOf[Type <: DFAny.Type, Mod <: DFAny.Modifier] : DFAny.Value[Type, Mod] =
      DFAny.Value[Type, Mod](this)
    final def asUninitialized[Type <: DFAny.Type, Mod <: DFAny.Modifier] : DFAny.Value[Type, Mod] with Dcl.Uninitialized =
      DFAny.Value[Type, Mod](this).asInstanceOf[DFAny.Value[Type, Mod] with Dcl.Uninitialized]
    final def asRefOwner[Type <: DFAny.Type, Mod <: DFAny.Modifier] : DFAny.Value[Type, Mod] with DFMember.RefOwner =
      DFAny.Value[Type, Mod](this).asInstanceOf[DFAny.Value[Type, Mod] with DFMember.RefOwner]
    override lazy val typeName: String = dfType.toString
    def isAssignable : Boolean = this match {
      case DFAny.Out() | DFAny.Var() => true
      case _ => false
    }
    def isPortOut : Boolean = this match {
      case DFAny.Out() => true
      case _ => false
    }
    def isPortIn : Boolean = this match {
      case DFAny.In() => true
      case _ => false
    }
    def isTopLevelInput(implicit getSet: MemberGetSet) : Boolean =
      isPortIn && getOwnerDesign.isTop
    def isTopLevelOutput(implicit getSet: MemberGetSet) : Boolean =
      isPortOut && getOwnerDesign.isTop
    def isPort : Boolean = isPortIn || isPortOut
  }

  trait Type extends Product with Serializable {
    type TToken <: DFAny.Token
    type Width
    val width : TwoFace.Int[Width]
    type TPattern <: DFAny.Pattern.Of[TPattern]
    type TPatternAble[+R] <: DFAny.Pattern.Able[R]
    type TPatternBuilder[LType <: Type] <: DFAny.Pattern.Builder[LType, TPatternAble]
    def getBubbleToken : TToken
    def getTokenFromBits(fromToken : DFBits.Token) : DFAny.Token
    def assignCheck(from : DFAny.Member)(implicit ctx : DFAny.Context) : Unit
    def codeString(implicit printer: CSPrinter) : String
  }

  trait TypeTag[T <: DFAny.Type]
  object Type {
    implicit class __TypeOps[Type <: DFAny.Type](val dfType : Type) {
      def <>[D <: DclDir](dir : D)(implicit ctx : DFAny.Context) : UninitializedDcl[Type] = {
        val modifier = dir.getModifier
        val newMember = Dcl(dfType, modifier, None, ctx.owner, ctx.meta)
        ctx.db.addMember(newMember).asUninitialized[Type, Modifier.Dcl]
      }
      def <>(defaultDir : DEFAULT_DIR)(implicit ctx : DFAny.Context) : UninitializedDcl[Type] = dfType <> defaultDir.get
    }
    implicit val __DFBoolType : DFBool.type => DFBool.Type = _ => DFBool.Type(true)
    implicit val __DFBitType : DFBit.type => DFBool.Type = _ => DFBool.Type(false)
    class Holder[T <: DFAny.Type, TT](t : TT)(implicit tc : TT => T) {
      type TType = T
      val dfType = tc(t)
    }
    implicit def ev[T <: DFAny](t : T) : t.TType = t.dfType
  }

  trait Frontend {
    protected implicit class __TypeOps2[Type <: DFAny.Type, T](val t : T)(implicit val tc : T => Type) {
      val dfType : Type = tc(t)
      def <>[D <: DclDir](dir : D)(implicit ctx : DFAny.Context) : UninitializedDcl[Type] = dfType <> dir
      def <>(defaultDir : DEFAULT_DIR)(implicit ctx : DFAny.Context) : UninitializedDcl[Type] = dfType <> defaultDir.get
      def init(token : DFAny.Token.ToFit.Conv[Type, dfType.TToken])(
        implicit ctx : DFAny.Context
      ) : DFAny.Of[Type] = trydf{DFAny.Const(dfType, token(dfType), named = true)}
      def ifdf[C, B](cond : Exact[C])(block : => Exact[B])(
        implicit ctx : DFBlock.Context, condArg : DFBool.Arg[C], blockConv : DFAny.`Op:=,<>`.Builder[Type, B]
      ) : DFConditional.WithRetVal.IfElseBlock[Type, true] = {
        val newMember = Dcl(dfType, Modifier.IfRetVar) setName ctx.meta.name //setting a RetVar modifier
        new DFConditional.WithRetVal.IfElseBlock[Type, true](newMember.asValModOf[Type, Modifier.NewVar], Some(condArg(cond)), None)(blockConv(dfType, block))
      }
      def matchdf[MVType <: DFAny.Type](matchValue : DFAny.Of[MVType], matchConfig : MatchConfig = MatchConfig.NoOverlappingCases)(
        implicit ctx : DFBlock.Context
      ): DFConditional.WithRetVal.MatchHeader[Type, MVType] = {
        val newMember = Dcl(dfType, Modifier.MatchRetVar) setName ctx.meta.name //setting a RetVar modifier
        new DFConditional.WithRetVal.MatchHeader[Type, MVType](newMember.asValModOf[Type, Modifier.NewVar], matchValue, matchConfig)
      }
      def X[N](cellNum : Positive.Checked[N]) : DFVector.Type[Type, N] = DFVector.Type(dfType, cellNum)
    }
  }
  object Frontend {
    trait Inherited extends Frontend {
      final override protected implicit def __TypeOps2[Type <: DFAny.Type, T](t : T)(implicit tc : T => Type) : __TypeOps2[Type, T] = super.__TypeOps2(t)
    }
    trait Imported extends Frontend {
      final override implicit def __TypeOps2[Type <: DFAny.Type, T](t : T)(implicit tc : T => Type) : __TypeOps2[Type, T] = super.__TypeOps2(t)
    }
  }

  @implicitNotFound(Context.MissingError.msg)
  trait Context extends DFMember.Context {
    val dir : DFDir
  }
  trait LowPriority {
    implicit def fromDefsCtx[T <: String with Singleton](implicit ctx : ContextOf[T], meta0 : Meta) : Context = new Context {
      val dir: DFDir = ctx.dir
      val meta: Meta = meta0
      val container: DFOwner.Container = ctx.container
      val db: DB.Mutable = ctx.db
    }
  }
  object Context extends LowPriority {
    final object MissingError extends ErrorMsg (
      "Missing an implicit owner Context.",
      "missing-context"
    ) {final val msg = getMsg}
    implicit def fromBlockCtx(implicit ctx : DFBlock.Context, meta0 : Meta) : Context = new Context {
      val dir: DFDir = ctx.dir
      val meta: Meta = meta0
      val container: DFOwner.Container = ctx.container
      val db: DB.Mutable = ctx.db
    }
  }

  type Ref[T <: Ref.Type] = DFMember.OwnedRef.Of[T, DFAny.Member]
  object Ref {
    trait Type extends DFMember.OwnedRef.Type
    implicit val ev : Type = new Type {}
    type ConsumeFrom = Ref[ConsumeFrom.Type]
    object ConsumeFrom {
      trait Type extends Ref.Type
      implicit val ev : Type = new Type {}
    }
    type ProduceTo = Ref[ProduceTo.Type]
    object ProduceTo {
      trait Type extends Ref.Type
      implicit val ev : Type = new Type {}
    }
  }

  /**
    * Any dataflow variable with explicit underlying type and modifier
    * @tparam Type The underlying type that defines the dataflow variable
    * @tparam Mod The modifier for the dataflow variable
    */
  sealed trait UnselectableValue[Type <: DFAny.Type, +Mod <: Modifier] extends DFAny {
    type TType = Type
    lazy val dfType : Type = member.dfType.asInstanceOf[Type]
    lazy val modifier : TMod = member.modifier.asInstanceOf[TMod]
    type TMod <: Mod

    //////////////////////////////////////////////////////////////////////////
    // Equality
    //////////////////////////////////////////////////////////////////////////
    /**
      * @return the dataflow comparison equality result.
      */
    final def == [R](right : R)(
      implicit ccs: CaseClassSkipper[`Op==`.Builder[DFAny.Of[Type], R]]
    ) = ccs(op => op(left.asValOf[Type], right), (left : Any) == (right : Any))
    /**
      * @return the dataflow comparison inequality result.
      */
    final def != [R](right : R)(
      implicit ccs: CaseClassSkipper[`Op!=`.Builder[DFAny.Of[Type], R]]
    ) = ccs(op => op(left.asValOf[Type], right), (left : Any) != (right : Any))
    //////////////////////////////////////////////////////////////////////////
  }


  trait Value[Type <: DFAny.Type, +Mod <: DFAny.Modifier] extends UnselectableValue[Type, Mod] with scala.Dynamic {
    def selectDynamic(name : String)(implicit ctx : DFAny.Context) : Any = macro Value.selectDynamicMacro[Type, Mod]
    def applyDynamic(name: String)(args: Any*): Any = macro Value.applyDynamicMacro[Type, Mod]
  }

  object Value {
    def apply[Type <: DFAny.Type, Mod <: DFAny.Modifier](_member : DFAny.Member) : Value[Type, Mod] =
      new Value[Type, Mod] {
        val member : DFAny.Member = _member
      }
    def selectDynamicMacro[T <: DFAny.Type : c.WeakTypeTag, Mod <: DFAny.Modifier : c.WeakTypeTag](
      c : whitebox.Context
    )(name : c.Tree)(ctx : c.Tree) : c.Tree =  {
      import c.universe._
      val typeTpe = weakTypeOf[T]
      val modTpe = weakTypeOf[Mod]
      val Literal(Constant(nameStr : String)) = name
      val checkedTree =
        if (typeTpe <:< typeOf[DFStruct.Type[_]]) q"${c.prefix.tree}.fields.${TermName(nameStr)}"
        else q"${c.prefix.tree}.asInstanceOf[DFiant.DFAny.UnselectableValue[$typeTpe,$modTpe]].${TermName(nameStr)}"
      val msgPattern1 = "(?s)value (.*) is not a member of (.*)did you mean (.*)\\?".r
      val msgPattern2 = "(?s)value (.*) is not a member of (.*)".r
      try {
        c.typecheck(checkedTree.duplicate)
      } catch {
        case e : TypecheckException => e.getMessage match {
          case msgPattern1(valueStr,_,suggestion) =>
            c.abort(c.enclosingPosition, s"value $valueStr is not a member of ${c.prefix.tree.tpe}\ndid you mean ${suggestion}?")
          case msgPattern2(valueStr,_) =>
            c.abort(c.enclosingPosition, s"value $valueStr is not a member of ${c.prefix.tree.tpe}")
          case msg =>
            c.abort(c.enclosingPosition, msg)
        }
      }
      checkedTree
    }
    def applyDynamicMacro[T <: DFAny.Type : c.WeakTypeTag, Mod <: DFAny.Modifier : c.WeakTypeTag](
      c : whitebox.Context
    )(name : c.Tree)(args : c.Tree*) : c.Tree =  {
      import c.universe._
      val typeTpe = weakTypeOf[T]
      val modTpe = weakTypeOf[Mod]
      val Literal(Constant(nameStr : String)) = name
      q"${c.prefix.tree}.asInstanceOf[DFiant.DFAny.UnselectableValue[$typeTpe,$modTpe]].${TermName(nameStr)}"
    }

    implicit class ValueOps[Type <: DFAny.Type, Mod <: DFAny.Modifier.Val](val left : Value[Type, Mod]) {
      type AsType[T <: DFAny.Type] = DFAny.Value[T, Mod]
      type AsVal = DFAny.Value[Type, DFAny.Modifier.Val]
      type AsVar = DFAny.VarOf[Type]
      import left.{member, width, Width, dfType, modifier}
      //////////////////////////////////////////////////////////////////////////
      // Bit range selection
      //////////////////////////////////////////////////////////////////////////
      /**
        * Bit Selection
        *
        * @param relBit relative bit index. Must be within bound of [0 : width-1]
        * @return the dataflow bit at the given index
        */
      final def bit[I](relBit : BitIndex.Checked[I, Width])(implicit ctx : DFAny.Context) : DFBool =
        trydf{DFAny.Alias.BitsWL.bit[Mod](member, relBit.unsafeCheck(width).getValue)}

      /**
        * @return the dataflow variable cast as bits vector
        */
      final def bits(implicit ctx : DFAny.Context) : DFBits[Width] = member match {
        case DFAny.Const(_, token, _, _) =>
          DFAny.Const.forced(token.bits).asInstanceOf[DFBits[Width]]
        case _ =>
          DFAny.Alias.BitsWL[Mod, Width](member, dfType.width, 0) tag cs"$left.bits"
      }

      final protected def protBits[H, L](relBitHigh : TwoFace.Int[H], relBitLow : TwoFace.Int[L])(
        implicit relWidth : RelWidth.TF[H, L], ctx : DFAny.Context
      ) : DFBits[relWidth.Out] = trydf {
        DFAny.Alias.BitsWL[Mod, relWidth.Out](member, relWidth(relBitHigh, relBitLow), relBitLow) tag
          cs"$left.bits(${CSFunc(_.LIT)}$relBitHigh, ${CSFunc(_.LIT)}$relBitLow)"
      }

      /**
        * Partial Bit Vector Selection between high and low indexes
        * @param relBitHigh relative high bit index.
        *                   Must be within bound of [0 : width-1].
        *                   Must be larger than `relBitLow`.
        * @param relBitLow relative low bit index
        *                   Must be within bound of [0 : width-1].
        *                   Must be smaller than `relBitHigh`.
        * @return the dataflow partial selected bits from the given vector
        */
      final def bits[H, L](relBitHigh : BitIndex.Checked[H, Width], relBitLow : BitIndex.Checked[L, Width])(
        implicit checkHiLow : BitsHiLo.CheckedShell[H, L], relWidth : RelWidth.TF[H, L], ctx : DFAny.Context
      ) : DFBits[relWidth.Out] = trydf {
        checkHiLow.unsafeCheck(relBitHigh, relBitLow)
        protBits(relBitHigh.unsafeCheck(width), relBitLow.unsafeCheck(width))
      }

      final protected def protBitsWL[W, L](relWidth : TwoFace.Int[W], relBitLow : TwoFace.Int[L])(
        implicit ctx : DFAny.Context
      ) : DFBits[W] = DFAny.Alias.BitsWL[Mod, W](member, relWidth, relBitLow)

      /**
        * Partial Bit Vector Selection at given low index and a relative width
        * @param relWidth relative selection width.
        *                 Must be within bound of [1 : width].
        * @param relBitLow relative low bit index
        *                   Must be within bound of [0 : width-1].
        * @return the dataflow partial selected bits from the given vector
        */
      final def bitsWL[W, L](relWidth : TwoFace.Int[W], relBitLow : BitIndex.Checked[L, Width])(
        implicit checkRelWidth : PartWidth.CheckedShell[W, Width - L], ctx : DFAny.Context
      ) : DFBits[W] = trydf {
        checkRelWidth.unsafeCheck(relWidth, width-relBitLow)
        protBitsWL(relWidth, relBitLow.unsafeCheck(width))
      }
      //////////////////////////////////////////////////////////////////////////

      //////////////////////////////////////////////////////////////////////////
      // Prev
      //////////////////////////////////////////////////////////////////////////
      final protected def protPrev(step : Int)(implicit ctx : DFAny.Context)
      : AsVal = DFAny.Alias.Prev[Type](member, step, DFAny.Alias.Prev.State)

      /**
        * @return a single step `prev`
        */
      final def prev(implicit ctx : DFAny.Context) : AsVal = protPrev(1)

      /**
        * The previous dataflow value at the n-th step.
        * @param step The step access into the dataflow variable history.
        *             Must be natural (0 steps create nothing).
        * @return the dataflow history at the chosen step.
        */
      final def prev[P](step : Natural.Int.Checked[P])(implicit ctx : DFAny.Context) : AsVal =
        trydf{protPrev(step)}
      //////////////////////////////////////////////////////////////////////////

      //////////////////////////////////////////////////////////////////////////
      // Pipe
      //////////////////////////////////////////////////////////////////////////
      final protected def protPipe(step : Int)(implicit ctx : DFAny.Context)
      : AsVal = DFAny.Alias.Prev[Type](member, step, DFAny.Alias.Prev.Pipe)

      /**
        * @return A single extra `pipe` step
        */
      final def pipe(implicit ctx : DFAny.Context) : AsVal = protPipe(1)

      /**
        * The dataflow with extra pipe steps. The difference between pipe and `prev` is that
        * the compiler can add/remove pipe stages automatically, since pipes are just constraints
        * on the datapath whereas `prev` is a functional access to the history state.
        * @param step The extra pipe steps required.
        *             Must be natural (0 steps create nothing).
        * @return the dataflow variable after extra steps of pipelining.
        */
      final def pipe[P](step : Natural.Int.Checked[P])(implicit ctx : DFAny.Context) : AsVal =
        trydf{protPipe(step)}
      //////////////////////////////////////////////////////////////////////////

      //////////////////////////////////////////////////////////////////////////
      // Opaque Casting
      //////////////////////////////////////////////////////////////////////////
      final def as[F <: DFOpaque.Fields](fields : F)(
        implicit ctx : DFAny.Context, sameType : fields.ActualType =:= Type, w : DFStruct.Fields.WidthOf[F]
      ) : DFAny.Value[DFOpaque.Type[F], Mod] = DFAny.Alias.AsIs(DFStruct.Type(fields), left)
      //////////////////////////////////////////////////////////////////////////

      //////////////////////////////////////////////////////////////////////////
      // Assignment
      //////////////////////////////////////////////////////////////////////////
      private type ImmutableMsg = "This operation cannot be applied on an immutable value"
      private type CheckMutable = RequireMsg[ImplicitFound[Mod <:< Modifier.Assignable], ImmutableMsg]
      def := [R](right : Exact[R])(
        implicit ctx : DFNet.Context, mutable : CheckMutable, op : left.Arg[R]
      ) : Value[Type, Mod] with FSM.Capable = trydf {
        left.assign(op(left.dfType, right))
        left.@@[FSM.Capable]
      }
      //////////////////////////////////////////////////////////////////////////

      /**
        * @return a new (mutable) dataflow variable matching the underlying type
        * of the current variable.
        */
      final def asNewVar(
        implicit ctx : DFAny.Context
      ) : DFAny.Value[Type, Modifier.NewVar] with Dcl.Uninitialized = NewVar(dfType)

      //////////////////////////////////////////////////////////////////////////
      // Dynamic Dataflow Control
      //////////////////////////////////////////////////////////////////////////
      final def fork(implicit ctx : DFAny.Context) : Value[Type, Modifier.Fork] = Fork(left)
      //fired only once for each new token
      final def isNotEmpty(implicit ctx : DFAny.Context) : DFBool = Dynamic.IsNotEmpty(left)
      final def isNotStall(implicit ctx : DFAny.Context) : DFBool = Dynamic.IsNotStall(left)
      //////////////////////////////////////////////////////////////////////////
    }
  }

  type Of[Type <: DFAny.Type] = Value[Type, Modifier.Val]

  sealed trait Modifier extends Product with Serializable {
    def codeString(implicit printer: CSPrinter) : String = ""
  }
  object Modifier {
    sealed trait Val extends Modifier
    case object Val extends Val
    sealed trait Fork extends Val
    case object Fork extends Fork
    sealed trait Assignable extends Val
    sealed trait Connectable extends Modifier
    sealed abstract class Dcl(dir : DclDir) extends Connectable with Assignable {
      override def codeString(implicit printer: CSPrinter) : String = {
        import printer.config._
        s" ${ALGN(1)}$DF<> $DF$dir"
      }
    }
    final case class Port(dir : PortDir) extends Dcl(dir)
    sealed abstract class NewVar() extends Dcl(VAR)
    case object DefaultDirVar extends NewVar
    case object NewVar extends NewVar
    sealed trait RetVar extends NewVar
    case object IfRetVar extends RetVar
    case object MatchRetVar extends RetVar
  }

  trait CanBeAnonymous extends DFMember

  protected[DFiant] final case class Init(seq : Seq[Token]) extends DFMember.CustomTagOf[DFAny.Member]
  protected[DFiant] final case class ConstValue(token : Token) extends DFMember.CustomTagOf[DFAny.Member]

  final protected[DFiant] implicit class AnyExtender(value : DFAny.Member) {
    def setInit(seq : Seq[Token])(implicit getSet : MemberGetSet) : DFAny.Member = value tag Init(seq)
    def clearInit(implicit getSet : MemberGetSet) : DFAny.Member = value.removeTagOf[Init]
    def getInit(implicit getSet : MemberGetSet) : Option[Seq[Token]] = value.getTagOf[Init].map(_.seq)
    def setConstValue(token : Token)(implicit getSet : MemberGetSet) : DFAny.Member = value tag ConstValue(token)
    def clearConstValue(implicit getSet : MemberGetSet) : DFAny.Member = value.removeTagOf[ConstValue]
    def getConstValue(implicit getSet : MemberGetSet) : Option[Token] = value.getTagOf[ConstValue].map(_.token)
  }

  final protected[DFiant] implicit class AnyExtender2[T](t : T)(implicit tc : T => DFAny.Member) {
    private val left = tc(t)
    def assign[T2](t2 : T2)(implicit ctx : DFNet.Context, tc2 : T2 => DFAny.Member) : DFNet = {
      val right = tc2(t2)
      if (left.isAssignable) DFNet.Assignment(left, right)
      else throw new IllegalArgumentException(
        s"""
           |Can only assign to a dataflow variable or an output port.
           |Attempted assignment: ${left.getFullName} := ${right.getFullName} at ${ctx.owner.getFullName}
           |""".stripMargin
      )
    }
  }

  final case class Const(
    dfType : DFAny.Type, token : DFAny.Token, ownerRef : DFOwner.Ref, tags : DFMember.Tags
  ) extends DFAny.Member with CanBeAnonymous {
    val modifier : Modifier = Modifier.Val

    protected[DFiant] def =~(that : DFMember)(implicit getSet : MemberGetSet) : Boolean = that match {
      case Const(dfType, token, _, tags) =>
        this.dfType == dfType && this.token == token && this.tags =~ tags
      case _ => false
    }
    def codeString(implicit printer: CSPrinter) : String = {
      import printer.config._
      s"${dfType.codeString} $DF init ${token.codeString}"
    }
    override def refCodeString(implicit printer: CSPrinter, owner : DFOwner) : String = {
      if (isAnonymous) token.codeString
      else super.refCodeString
    }
    override def show(implicit printer: CSPrinter) : String = s"Const($token) : $dfType"

    def setTags(tagsFunc : DFMember.Tags => DFMember.Tags)(implicit getSet : MemberGetSet) : DFMember =
      getSet.set(this)(m => m.copy(tags = tagsFunc(m.tags)))
  }
  object Const {
    def apply[Type <: DFAny.Type](dfType: Type, token: Type#TToken, named : Boolean = false)(implicit ctx: Context)
    : DFAny.Of[Type] = {
      val meta = if (named) ctx.meta else ctx.meta.anonymize
      ctx.db.addMember(Const(dfType, token, ctx.owner, meta)).asValOf[Type]
    }
    private[DFiant] def forced[Type <: DFAny.Type](token: DFAny.Token)(implicit ctx: Context)
    : DFAny.Of[Type] = ctx.db.addMember(Const(token.dfType, token, ctx.owner, ctx.meta.anonymize)).asValOf[Type]
    trait ToFit[Type <: DFAny.Type, V] {
      def apply(dfType : Type, value : V) : DFAny.Of[Type]
    }
    object ToFit {
      implicit def fromValueOf[Type <: DFAny.Type, V](
        implicit
        ctx : DFAny.Context,
        const : ToFit[Type, V]
      ) : ToFit[Type, ValueOf[V]] = (dfType, value) => const(dfType, value.value)
      implicit def ev[Type <: DFAny.Type, V](
        implicit
        ctx : DFAny.Context,
        tokenSummon : DFAny.Token.ToFit.Summon[Type, V, Type#TToken]
      ) : ToFit[Type, V] = (dfType, value) => Const(dfType, tokenSummon(dfType, value))
    }
    trait AsIs[-Type <: DFAny.Type, V] {
      type Out <: DFAny
      def apply(dfType : Type, value : V) : Out
    }
    object AsIs {
      type Aux[-Type <: DFAny.Type, V, TypeOut <: DFAny.Type] = AsIs[Type, V]{type Out = DFAny.Of[TypeOut]}
      implicit def fromValueOf[Type <: DFAny.Type, V, TypeOut <: DFAny.Type](
        implicit
        ctx : DFAny.Context,
        const : Aux[Type, V, TypeOut]
      ) : Aux[Type, ValueOf[V], TypeOut] = new AsIs[Type, ValueOf[V]] {
        type Out = DFAny.Of[TypeOut]
        def apply(dfType : Type, value : ValueOf[V]) : Out = const(dfType, value.value)
      }
      implicit def ev[Type <: DFAny.Type, V, TypeOut <: DFAny.Type](
        implicit
        ctx : DFAny.Context,
        tokenSummon : DFAny.Token.AsIs.Summon.Aux[Type, V, Type#TToken, DFAny.TokenT[Type#TToken, TypeOut]]
      ) : Aux[Type, V, TypeOut]  = new AsIs[Type, V] {
        type Out = DFAny.Of[TypeOut]
        def apply(dfType : Type, value : V) : Out = trydf {
          val token = tokenSummon(dfType, value)
          Const(token.dfType.asInstanceOf[TypeOut], token.asInstanceOf[TypeOut#TToken])
        }
      }
    }
  }

  final case class Dcl(
    dfType : DFAny.Type, modifier : DFAny.Modifier, externalInit : Option[Seq[DFAny.Token]], ownerRef : DFOwner.Ref, tags : DFMember.Tags
  ) extends DFAny.Member {
    protected[DFiant] def =~(that : DFMember)(implicit getSet : MemberGetSet) : Boolean = that match {
      case Dcl(dfType, modifier, externalInit, _, tags) =>
        this.dfType == dfType && this.modifier == modifier && this.externalInit == externalInit && this.tags =~ tags
      case _ => false
    }
    override def refCodeString(implicit printer: CSPrinter, owner: DFOwner): String = getOwnerBlock match {
      case DFDesign.Block.Internal(_,_,_,Some(rep)) => rep.inlineCodeString
      case _ => super.refCodeString(printer, owner)
    }
    def codeString(implicit printer: CSPrinter) : String = {
      import printer.config._
      val initStr = externalInit match {
        case Some(token +: Nil) => s" $DF init ${token.codeString}"
        case Some(tokens) => s" $DF init ${tokens.codeString}"
        case None => ""
      }
      s"${dfType.codeString}${modifier.codeString}$initStr"
    }
    override lazy val typeName: String = modifier match {
      case Modifier.Port(IN) => s"$dfType <> IN"
      case Modifier.Port(OUT) => s"$dfType <> OUT"
      case _ => s"$dfType <> VAR"
    }

    def setTags(tagsFunc : DFMember.Tags => DFMember.Tags)(implicit getSet : MemberGetSet) : DFMember = getSet.set(this)(m => m.copy(tags = tagsFunc(m.tags)))
  }
  object Dcl {
    trait Uninitialized
    implicit class InitializableOps[Type <: DFAny.Type, Mod <: DFAny.Modifier](val i : Value[Type, Mod] with Uninitialized) {
      def init[T](tokens : DFAny.Token.ToFit.Conv[Type, i.dfType.TToken]*)(
        implicit ctx : DFAny.Context
      ) : Value[Type, Mod] = trydf{forcedInit(tokens.map(_.apply(i.dfType)))}

      private[DFiant] def forcedInit(tokens : Seq[Token])(implicit ctx : DFAny.Context) : Value[Type, Mod] = {
        val newMember = Dcl(i.dfType, i.modifier, Some(tokens), ctx.owner, ctx.meta)
        if (ctx.meta.namePosition == i.member.tags.meta.namePosition) {
          implicitly[MemberGetSet].set[DFAny.Member](i.member)(_ => newMember)
          newMember.asValModOf[Type, Mod]
        } else ctx.db.addMember(newMember).asValModOf[Type, Mod]
      }
    }
    def apply[Type <: DFAny.Type, Mod <: DFAny.Modifier](
      dfType: Type, modifier : Mod, externalInit : Option[Seq[DFAny.Token]] = None
    )(
      implicit ctx: DFAny.Context
    ): Value[Type, Mod] with Uninitialized = {
      val specifyDefaultModifier = modifier match {
        //Using a default dir modifier
        case Modifier.DefaultDirVar => ctx.container match {
          //The container is an interface, so the modifier may be different than NewVar
          case ifc : DFInterface => ifc.DEFAULT_DIR.get match {
            case dir : PortDir => Modifier.Port(dir)
            case DFiant.VAR => Modifier.NewVar
          }
          //The container does not support a default direction override
          case _ => Modifier.NewVar
        }
        //Not a default dir modifier -> leave it be
        case _ => modifier
      }
      val actualModifier : Modifier = ctx.dir match {
        case IN => Modifier.Port(IN)
        case OUT => Modifier.Port(OUT)
        case VAR => Modifier.NewVar
        case FLIP => specifyDefaultModifier match {
          case Modifier.Port(IN) => Modifier.Port(OUT)
          case Modifier.Port(OUT) => Modifier.Port(IN)
          case _ => specifyDefaultModifier
        }
        case ASIS => specifyDefaultModifier
      }
      ctx.db.addMember(Dcl(dfType, actualModifier, externalInit, ctx.owner, ctx.meta)).asUninitialized[Type, Mod]
    }
  }
  type UninitializedDcl[Type <: DFAny.Type] = Value[Type, Modifier.Dcl] with Dcl.Uninitialized
  type DclOf[Type <: DFAny.Type] = Value[Type, Modifier.Dcl]
  implicit class DclOps1[Type <: DFAny.Type](left : DclOf[Type]) {
    def <>[RType <: DFAny.Type](right: DFAny.Of[RType])(
      implicit ctx: DFNet.Context, op: left.Arg[DFAny.Of[RType]]
    ): DFNet = trydf{left.connect(op(left.dfType, right))}
    def <>[R](right: Exact[R])(
      implicit ctx: DFNet.Context, op: left.Arg[R]
    ): DFNet = trydf{left.connect(op(left.dfType, right))}
  }
  implicit class DclOps2[L](left : L) {
    def <>[Type <: DFAny.Type](right: DclOf[Type])(
      implicit ctx: DFNet.Context, op: right.Arg[L]
    ): DFNet = trydf{right.connect(op(right.dfType, left))}
  }
  implicit class DclOps3[Type <: DFAny.Type](left : Of[Type]) {
    def <>(right: DclOf[Type])(
      implicit ctx: DFNet.Context, op: left.Arg[Of[Type]]
    ): DFNet = trydf{right.connect(op(right.dfType, left))}
  }

  object Port {
    object In {
      def apply[Type <: DFAny.Type](dfType: Type)(
        implicit ctx: DFAny.Context
      ) : UninitializedPort[Type] = Dcl(dfType, Modifier.Port(IN))
      def unapply(arg: Dcl) : Boolean = arg.modifier match {
        case Modifier.Port(IN) => true
        case _ => false
      }
    }
    object Out {
      def apply[Type <: DFAny.Type](dfType: Type)(
        implicit ctx: DFAny.Context
      ) : UninitializedPort[Type] = Dcl(dfType, Modifier.Port(OUT))
      def unapply(arg: Dcl) : Boolean = arg.modifier match {
        case Modifier.Port(OUT) => true
        case _ => false
      }
    }
  }
  type UninitializedPort[Type <: DFAny.Type] = Value[Type, Modifier.Port] with Dcl.Uninitialized

  object NewVar {
    def apply[Type <: DFAny.Type](dfType: Type)(
      implicit ctx: Context
    ) : UninitializedNewVar[Type] = Dcl[Type, Modifier.NewVar](dfType, Modifier.DefaultDirVar)
    def unapply(arg: Dcl) : Boolean = arg.modifier match {
      case Modifier.NewVar => true
      case _ => false
    }
  }
  type UninitializedNewVar[Type <: DFAny.Type] = Value[Type, Modifier.NewVar] with Dcl.Uninitialized

  sealed trait Alias extends DFAny.Member with CanBeAnonymous {
    val relValRef : Alias.RelValRef
    def constFunc(t : DFAny.Token) : DFAny.Token
    def initFunc(t : Seq[DFAny.Token]) : Seq[DFAny.Token] = TokenSeq(t)(constFunc)
    def relCodeString(cs : String)(implicit printer: CSPrinter) : String
    def codeString(implicit printer: CSPrinter): String = {
      this.getTagOf[CompactCodeString] match {
        case Some(ccs) => ccs.codeString
        case None => relCodeString(relValRef.refCodeString.applyBrackets())
      }
    }
  }
  object Alias {
    type RelValRef = DFAny.Ref[RelValRef.Type]
    object RelValRef {
      trait Type extends DFAny.Ref.Type
      implicit val ev : Type = new Type {}
    }

    final case class AsIs(
      dfType : Type, modifier : Modifier, relValRef : RelValRef, ownerRef : DFOwner.Ref, tags : DFMember.Tags
    ) extends Alias {
      type TMod = Modifier
      protected[DFiant] def =~(that : DFMember)(implicit getSet : MemberGetSet) : Boolean = that match {
        case AsIs(dfType, modifier, relValRef, _, tags) =>
          this.dfType == dfType && this.modifier == modifier && this.relValRef =~ relValRef && this.tags =~ tags
        case _ => false
      }
      def constFunc(t : DFAny.Token) : DFAny.Token = {
        if (t.dfType.width.getValue != dfType.width.getValue) t match {
          case b : DFBits.Token => b.resize(dfType.width)
          case d @ DFDecimal.Token(_, width, fractionWidth, _) if fractionWidth == 0 => d.resize(dfType.width)
        } else dfType.getTokenFromBits(t.bits)
      }
      def relCodeString(cs : String)(implicit printer: CSPrinter) : String = {
        import printer.config._
        s"$cs.$DF as(${dfType.codeString})"
      }

      def setTags(tagsFunc : DFMember.Tags => DFMember.Tags)(implicit getSet : MemberGetSet) : DFMember = getSet.set(this)(m => m.copy(tags = tagsFunc(m.tags)))
    }
    object AsIs {
      def apply[Type <: DFAny.Type, RelVal <: DFAny](dfType: Type, relVal: RelVal)(
        implicit ctx: Context
      ): Value[Type, relVal.TMod] = {
        implicit lazy val ret : Value[Type, relVal.TMod] with DFMember.RefOwner =
          ctx.db.addMember(AsIs(dfType, relVal.modifier, relVal.member, ctx.owner, ctx.meta)).asRefOwner[Type, relVal.TMod]
        ret
      }
      object Unref {
        def unapply(arg : AsIs)(implicit getSet: MemberGetSet) : Option[(Type, Modifier, DFAny.Member, DFOwner, DFMember.Tags)] =
          Some(arg.dfType, arg.modifier, arg.relValRef.get, arg.ownerRef.get, arg.tags)
      }
    }
    final case class BitsWL(
      dfType : Type, modifier : Modifier, relValRef : RelValRef, relWidth : Int, relBitLow : Int, ownerRef : DFOwner.Ref, tags : DFMember.Tags
    ) extends Alias{
      protected[DFiant] def =~(that : DFMember)(implicit getSet : MemberGetSet) : Boolean = that match {
        case BitsWL(dfType, modifier, relValRef, relWidth, relBitLow, _, tags) =>
          this.dfType == dfType && this.modifier == modifier && this.relWidth == relWidth &&
            this.relBitLow == relBitLow && this.relValRef =~ relValRef && this.tags =~ tags
        case _ => false
      }
      def constFunc(t : DFAny.Token) : DFAny.Token = dfType match {
        case _ : DFBits.Type[_] => t.bitsWL(relWidth, relBitLow)
        case _ : DFBool.Type => t.bit(relBitLow)
      }
      def relCodeString(cs : String)(implicit printer: CSPrinter) : String = {
        import printer.config._
        dfType match {
          case _ : DFBits.Type[_] => s"$cs.$DF bitsWL($LIT$relWidth, $LIT$relBitLow)"
          case _ : DFBool.Type => s"$cs.$DF bit($LIT$relBitLow)"
        }
      }

      def setTags(tagsFunc : DFMember.Tags => DFMember.Tags)(implicit getSet : MemberGetSet) : DFMember = getSet.set(this)(m => m.copy(tags = tagsFunc(m.tags)))
    }
    object BitsWL {
      def apply[RelValMod <: Modifier, W](relVal: DFAny.Member, relWidth: TwoFace.Int[W], relBitLow: Int)(
        implicit ctx: Context
      ): Value[DFBits.Type[W], RelValMod] = {
        implicit lazy val ret : Value[DFBits.Type[W], RelValMod] with DFMember.RefOwner =
          ctx.db.addMember(
            BitsWL(DFBits.Type(relWidth), relVal.modifier, relVal, relWidth, relBitLow, ctx.owner, ctx.meta)
          ).asRefOwner[DFBits.Type[W], RelValMod]
        ret
      }
      def bit[RelValMod <: Modifier](relVal: DFAny.Member, relBit: Int)(
        implicit ctx: Context
      ): Value[DFBool.Type, RelValMod] = {
        implicit lazy val ret : Value[DFBool.Type, RelValMod] with DFMember.RefOwner =
          ctx.db.addMember(
            BitsWL(DFBool.Type(logical = false), relVal.modifier, relVal, 1, relBit, ctx.owner, ctx.meta)
          ).asRefOwner[DFBool.Type, RelValMod]
        ret
      }
      object Unref {
        def unapply(arg : BitsWL)(implicit getSet: MemberGetSet) : Option[(Type, Modifier, DFAny.Member, Int, Int, DFOwner, DFMember.Tags)] =
          Some(arg.dfType, arg.modifier, arg.relValRef.get, arg.relWidth, arg.relBitLow, arg.ownerRef.get, arg.tags)
      }
    }
    final case class Prev(
      dfType : Type, relValRef : RelValRef, step : Int, kind : Prev.Kind, ownerRef : DFOwner.Ref, tags : DFMember.Tags
    ) extends Alias {
      val modifier : Modifier = Modifier.Val
      protected[DFiant] def =~(that : DFMember)(implicit getSet : MemberGetSet) : Boolean = that match {
        case Prev(dfType, relValRef, step, kind, _, tags) =>
          this.dfType == dfType && this.relValRef =~ relValRef &&
            this.step == step && this.kind == kind && this.tags =~ tags
        case _ => false
      }
      def constFunc(t : DFAny.Token) : DFAny.Token = t
      override def initFunc(t : Seq[DFAny.Token]) : Seq[DFAny.Token] = t.prevInit(step)
      def relCodeString(cs : String)(implicit printer: CSPrinter) : String = {
        import printer.config._
        if (step == 1) s"$cs.$DF ${kind.op}" else s"$cs.$DF ${kind.op}($LIT$step)"
      }

      def setTags(tagsFunc : DFMember.Tags => DFMember.Tags)(implicit getSet : MemberGetSet) : DFMember = getSet.set(this)(m => m.copy(tags = tagsFunc(m.tags)))
    }
    object Prev {
      sealed trait Kind extends Product with Serializable {
        def op : String
      }
      case object State extends Kind {
        def op : String = "prev"
      }
      case object Pipe extends Kind {
        def op : String = "pipe"
      }
      def apply[RelValType <: DFAny.Type](relVal: DFAny.Member, step: Int, kind : Kind)(
        implicit ctx: Context
      ): Value[RelValType, Modifier.Val] = {
        implicit lazy val ret : Value[RelValType, Modifier.Val] with DFMember.RefOwner =
          ctx.db.addMember(
            Prev(relVal.dfType, relVal, step, kind, ctx.owner, ctx.meta)
          ).asRefOwner[RelValType, Modifier.Val]
        ret
      }
      object Unref {
        def unapply(arg : Prev)(
          implicit getSet: MemberGetSet
        ) : Option[(Type, DFAny.Member, Int, Kind, DFOwner, DFMember.Tags)] =
          Some(arg.dfType, arg.relValRef.get, arg.step, arg.kind, arg.ownerRef.get, arg.tags)
      }
    }
  }


  final case class ApplySel(
    dfType : Type, modifier : Modifier, relValRef : ApplySel.RelValRef, idxRef : ApplySel.IdxRef, ownerRef : DFOwner.Ref, tags : DFMember.Tags
  ) extends DFAny.Member with CanBeAnonymous {
    protected[DFiant] def =~(that : DFMember)(implicit getSet : MemberGetSet) : Boolean = that match {
      case ApplySel(dfType, modifier, relValRef, idxRef, _, tags) =>
        this.dfType == dfType && this.modifier == modifier && this.idxRef =~ idxRef &&
          this.relValRef =~ relValRef && this.tags =~ tags
      case _ => false
    }
    def codeString(implicit printer: CSPrinter) : String = {
      import printer.config._
      s"${relValRef.refCodeString}(${idxRef.refCodeString})"
    }

    def setTags(tagsFunc : DFMember.Tags => DFMember.Tags)(implicit getSet : MemberGetSet) : DFMember =
      getSet.set(this)(m => m.copy(tags = tagsFunc(m.tags)))
  }
  object ApplySel {
    type RelValRef = DFAny.Ref[RelValRef.Type]
    object RelValRef {
      trait Type extends DFAny.Ref.Type
      implicit val ev : Type = new Type {}
    }
    type IdxRef = DFAny.Ref[IdxRef.Type]
    object IdxRef {
      trait Type extends DFAny.Ref.ConsumeFrom.Type
      implicit val ev : Type = new Type {}
    }

    def fromArray[Type <: DFAny.Type, N, Mod <: DFAny.Modifier](relVal: Value[DFVector.Type[Type, N], Mod], idxVal : DFAny.Member)(
      implicit ctx: Context
    ): Value[Type, Mod] = {
      implicit lazy val ret : Value[Type, Mod] with DFMember.RefOwner =
        ctx.db.addMember(
          ApplySel(relVal.dfType.cellType, relVal.modifier, relVal.member, idxVal, ctx.owner, ctx.meta.anonymize)
        ).asRefOwner[Type, Mod]
      ret
    }
    def fromBits[W, Mod <: DFAny.Modifier](relVal: Value[DFBits.Type[W], Mod], idxVal: DFAny.Member)(
      implicit ctx: Context
    ): Value[DFBool.Type, Mod] = {
      implicit lazy val ret : Value[DFBool.Type, Mod] with DFMember.RefOwner =
        ctx.db.addMember(
          ApplySel(DFBool.Type(logical = false), relVal.modifier, relVal.member, idxVal, ctx.owner, ctx.meta.anonymize)
        ).asRefOwner[DFBool.Type, Mod]
      ret
    }
    object Unref {
      def unapply(arg : ApplySel)(
        implicit getSet: MemberGetSet
      ) : Option[(Type, Modifier, DFAny.Member, DFAny.Member, DFOwner, DFMember.Tags)] =
        Some(arg.dfType, arg.modifier, arg.relValRef.get, arg.idxRef.get, arg.ownerRef.get, arg.tags)
    }
  }


  sealed abstract class Func extends DFAny.Member with CanBeAnonymous {
    val modifier : Modifier = Modifier.Val
  }
  final case class Func1(
    dfType: Type, leftArgRef : Func1.Ref.LeftArg, op : Func1.Op, ownerRef : DFOwner.Ref, tags : DFMember.Tags
  )(val tokenFunc : Token => Token) extends Func {
    val initFunc : Seq[DFAny.Token] => Seq[DFAny.Token] = l => TokenSeq(l)(tokenFunc)
    protected[DFiant] def =~(that : DFMember)(implicit getSet : MemberGetSet) : Boolean = that match {
      case Func1(dfType, leftArgRef, op, _, tags) =>
        this.dfType == dfType && this.leftArgRef =~ leftArgRef && this.op == op && this.tags =~ tags
      case _ => false
    }
    def codeString(implicit printer: CSPrinter) : String = {
      if (op.toString.startsWith("unary_")) s"${op.toString.last}${leftArgRef.refCodeString.applyBrackets()}"
      else s"${leftArgRef.refCodeString.applyBrackets()}.$op"
    }
    override def show(implicit printer: CSPrinter) : String = s"$codeString : $dfType"

    def setTags(tagsFunc : DFMember.Tags => DFMember.Tags)(implicit getSet : MemberGetSet) : DFMember =
      getSet.set(this)(m => m.copy(tags = tagsFunc(m.tags))(tokenFunc))
  }
  object Func1 {
    type Ref[T <: Ref.Type] = DFAny.Ref[T]
    object Ref {
      trait Type extends DFAny.Ref.ConsumeFrom.Type
      type LeftArg = Ref[LeftArg.Type]
      object LeftArg {
        trait Type extends Ref.Type
        implicit val ev : Type = new Type {}
      }
    }
    sealed trait Op
    object Op {
      sealed trait unary_- extends Op
      sealed trait Invert extends Op
      sealed trait unary_! extends Invert
      sealed trait unary_~ extends Invert
      sealed trait reverse extends Op
      implicit object unary_- extends unary_-
      implicit object unary_! extends unary_!
      implicit object unary_~ extends unary_~
      implicit object reverse extends reverse
    }
    def apply[Type <: DFAny.Type, L <: DFAny, Op <: Func1.Op](
      dfType: Type, leftArg: L, op: Op
    )(func: L#TToken => Type#TToken)(implicit ctx: Context) : DFAny.Of[Type] = {
      val func0 : DFAny.Token => DFAny.Token = l => func(l.asInstanceOf[L#TToken])
      forced(dfType, leftArg, op)(func0)
    }
    private[DFiant] def forced[Type <: DFAny.Type](
      dfType: Type, leftArg: DFAny.Member, op: Func1.Op
    )(tokenFunc: (_ <: Token) => Token)(implicit ctx: Context) : DFAny.Of[Type] = {
      implicit lazy val ret : DFAny.Of[Type] with DFMember.RefOwner =
        ctx.db.addMember(
          Func1(dfType, leftArg, op, ctx.owner, ctx.meta)(tokenFunc.asInstanceOf[Token => Token])
        ).asRefOwner[Type, Modifier.Val]
      ret
    }
    object Unref {
      def unapply(arg : Func1)(
        implicit getSet: MemberGetSet
      ) : Option[(Type, DFAny.Member, Op, DFOwner, DFMember.Tags)] = arg match {
        case Func1(dfType, leftArgRef, op, ownerRef, tags) =>
          Some((dfType, leftArgRef.get, op, ownerRef.get, tags))
        case _ => None
      }
    }
  }

  final case class Func2(
    dfType: Type, leftArgRef : Func2.Ref.LeftArg, op : Func2.Op, rightArgRef : Func2.Ref.RightArg,
    ownerRef : DFOwner.Ref, tags : DFMember.Tags
  )(val tokenFunc : (Token, Token) => Token) extends Func {
    val initFunc : (Seq[DFAny.Token], Seq[DFAny.Token]) => Seq[DFAny.Token] = (l, r) => TokenSeq(l, r)(tokenFunc)
    protected[DFiant] def =~(that : DFMember)(implicit getSet : MemberGetSet) : Boolean = that match {
      case Func2(dfType, leftArgRef, op, rightArgRef, _, tags) =>
        this.dfType == dfType && this.leftArgRef =~ leftArgRef && this.op == op &&
          this.rightArgRef =~ rightArgRef && this.tags =~ tags
      case _ => false
    }
    def codeString(implicit printer: CSPrinter) : String = {
      s"${leftArgRef.refCodeString.applyBrackets()} $op ${rightArgRef.refCodeString.applyBrackets()}"
    }
    override def show(implicit printer: CSPrinter) : String = s"$codeString : $dfType"

    def setTags(tagsFunc : DFMember.Tags => DFMember.Tags)(implicit getSet : MemberGetSet) : DFMember =
      getSet.set(this)(m => m.copy(tags = tagsFunc(m.tags))(tokenFunc))
  }
  object Func2 {
    type Ref[T <: Ref.Type] = DFAny.Ref[T]
    object Ref {
      trait Type extends DFAny.Ref.ConsumeFrom.Type
      type LeftArg = Ref[LeftArg.Type]
      object LeftArg {
        trait Type extends Ref.Type
        implicit val ev : Type = new Type {}
      }
      type RightArg = Ref[RightArg.Type]
      object RightArg {
        trait Type extends Ref.Type
        implicit val ev : Type = new Type {}
      }
    }

    sealed trait Op
    //Dual Input, Single Output Operation
    object Op {
      sealed trait OptionalCarry extends Op {
        type WC <: XBoolean
      }
      sealed trait NoCarry extends OptionalCarry {
        type WC = false
      }
      sealed trait Carry extends OptionalCarry {
        type WC = true
      }
      sealed trait Negateable extends OptionalCarry {
        def negate : Negateable
      }
      sealed trait +  extends Negateable with NoCarry {
        def negate : - = -
      }
      sealed trait -  extends Negateable with NoCarry {
        def negate : + = +
      }
      sealed trait *  extends NoCarry
      sealed trait /  extends NoCarry
      sealed trait +^  extends Negateable with Carry {
        def negate : -^ = -^
      }
      sealed trait -^  extends Negateable with Carry {
        def negate : +^ = +^
      }
      sealed trait *^  extends OptionalCarry with Carry
      sealed trait Equality extends Op
      sealed trait == extends Equality {
        override def toString: String = "==="
      }
      sealed trait != extends Equality {
        override def toString: String = "=!="
      }
      sealed trait <  extends Op
      sealed trait >  extends Op
      sealed trait <= extends Op
      sealed trait >= extends Op
      sealed trait |  extends Op
      sealed trait &  extends Op
      sealed trait ^  extends Op
      sealed trait Shift extends Op
      sealed trait << extends Shift
      sealed trait >> extends Shift
      sealed trait || extends Op
      sealed trait && extends Op
      sealed trait ++ extends Op
      implicit case object +  extends +
      implicit case object -  extends -
      implicit case object *  extends *
      implicit case object /  extends /
      implicit case object +^  extends +^
      implicit case object -^  extends -^
      implicit case object *^  extends *^
      implicit case object == extends ==
      implicit case object != extends !=
      implicit case object <  extends <
      implicit case object >  extends >
      implicit case object <= extends <=
      implicit case object >= extends >=
      implicit case object |  extends |
      implicit case object &  extends &
      implicit case object ^  extends ^
      implicit case object << extends <<
      implicit case object >> extends >>
      implicit case object || extends ||
      implicit case object && extends &&
      implicit case object ++ extends ++
    }

    def apply[Type <: DFAny.Type, L <: DFAny, Op <: Func2.Op, R <: DFAny](
      dfType: Type, leftArg: L, op: Op, rightArg: R
    )(func: (L#TToken, R#TToken) => Type#TToken)(implicit ctx: Context) : DFAny.Of[Type] = {
      val func0 : (DFAny.Token, DFAny.Token) => DFAny.Token =
        (l, r) => func(l.asInstanceOf[L#TToken], r.asInstanceOf[R#TToken])
      forced(dfType, leftArg, op, rightArg)(func0)
    }
    private[DFiant] def forced[Type <: DFAny.Type](
      dfType: Type, leftArg: DFAny.Member, op: Func2.Op, rightArg: DFAny.Member
    )(tokenFunc: (_ <: Token, _ <: Token) => Token)(implicit ctx: Context) : DFAny.Of[Type] = {
      implicit lazy val ret : DFAny.Of[Type] with DFMember.RefOwner =
        ctx.db.addMember(
          Func2(dfType, leftArg, op, rightArg, ctx.owner, ctx.meta)(tokenFunc.asInstanceOf[(Token, Token) => Token])
        ).asRefOwner[Type, Modifier.Val]
      ret
    }
    object Unref {
      def unapply(arg : Func2)(
        implicit getSet: MemberGetSet
      ) : Option[(Type, DFAny.Member, Op, DFAny.Member, DFOwner, DFMember.Tags)] = arg match {
        case Func2(dfType, leftArgRef, op, rightArgRef, ownerRef, tags) =>
          Some((dfType, leftArgRef.get, op, rightArgRef.get, ownerRef.get, tags))
        case _ => None
      }
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Dynamic Dataflow Constructs
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  final case class Fork(
    dfType : Type, relValRef : Fork.Ref, ownerRef : DFOwner.Ref, tags : DFMember.Tags
  ) extends DFAny.Member {
    val modifier : Modifier = Modifier.Fork
    protected[DFiant] def =~(that : DFMember)(implicit getSet : MemberGetSet) : Boolean = that match {
      case Fork(dfType, relValRef, _, tags) =>
        this.dfType == dfType && this.relValRef =~ relValRef && this.tags =~ tags
      case _ => false
    }
    def codeString(implicit printer: CSPrinter): String = {
      import printer.config._
      s"${relValRef.refCodeString.applyBrackets()}.$DF fork"
    }

    def setTags(tagsFunc : DFMember.Tags => DFMember.Tags)(implicit getSet : MemberGetSet) : DFMember =
      getSet.set(this)(m => m.copy(tags = tagsFunc(m.tags)))
  }
  object Fork {
    type Ref = DFMember.OwnedRef.Of[Ref.Type, DFAny.Member]
    object Ref {
      trait Type extends DFAny.Ref.ConsumeFrom.Type
      implicit val ev : Type = new Type {}
    }
    def apply[RelVal <: DFAny](relVal: RelVal)(
      implicit ctx: DFAny.Context
    ): Value[RelVal#TType, Modifier.Fork] = {
      implicit lazy val ret : Value[RelVal#TType, Modifier.Fork] with DFMember.RefOwner =
        ctx.db.addMember(
          Fork(relVal.dfType, relVal.member, ctx.owner, ctx.meta)
        ).asRefOwner[RelVal#TType, Modifier.Fork]
      ret
    }
    object Unref {
      def unapply(arg : Fork)(
        implicit getSet: MemberGetSet
      ) : Option[(DFAny.Type, DFAny.Member, DFOwner, DFMember.Tags)] =
        Some((arg.dfType, arg.relValRef.get, arg.ownerRef.get, arg.tags))
    }
  }
  implicit class ForkOps[Type <: DFAny.Type](val left : Value[Type, Modifier.Fork]) {
    final def dontConsume()(implicit ctx : DFAny.Context) : Unit = Dynamic.DontConsume(left)
    final def consume()(implicit ctx : DFAny.Context) : Unit = Dynamic.Consume(left)
  }
  //Dynamic is a always boolean for implementation simplification. Only `IsNotEmpty` and `IsNotFull` are
  //useable as boolean values.
  final case class Dynamic(
    relValRef : Dynamic.Ref, func : Dynamic.Func, ownerRef : DFOwner.Ref, tags : DFMember.Tags
  ) extends DFAny.Member with CanBeAnonymous {
    val modifier : Modifier = Modifier.Val
    val dfType : DFBool.Type = DFBool.Type(true)
    protected[DFiant] def =~(that : DFMember)(implicit getSet : MemberGetSet) : Boolean = that match {
      case Dynamic(relValRef, func, _, tags) =>
        this.func == func && this.relValRef =~ relValRef && this.tags =~ tags
      case _ => false
    }
    def codeString(implicit printer: CSPrinter): String = {
      import printer.config._
      s"${relValRef.refCodeString.applyBrackets()}.$DF ${func.codeString}"
    }

    def setTags(tagsFunc : DFMember.Tags => DFMember.Tags)(implicit getSet : MemberGetSet) : DFMember =
      getSet.set(this)(m => m.copy(tags = tagsFunc(m.tags)))
  }
  object Dynamic {
    type Ref = DFAny.Ref[Ref.Type]
    object Ref {
      trait Type extends DFAny.Ref.Type
      implicit val ev : Type = new Type {}
    }
    sealed trait Func extends Product with Serializable {
      def codeString : String
    }
    object Func {
      sealed trait Status extends Func
      case object IsNotEmpty extends Status {
        def codeString : String = "isNotEmpty"
      }
      case object IsNotFull extends Status {
        def codeString : String = "isNotFull"
      }
      case object IsNotStall extends Status {
        def codeString : String = "isNotStall"
      }
      sealed trait Control extends Func
      case object Consume extends Control {
        def codeString : String = "consume()"
      }
      case object DontConsume extends Control {
        def codeString : String = "dontConsume()"
      }
      type DontProduce = DontProduce.type
      case object DontProduce extends Control {
        def codeString : String = "dontProduce()"
      }
    }

    def apply(relVal: DFAny.Member, func : Func)(
      implicit ctx: DFAny.Context
    ): DFBool = {
      implicit lazy val ret : DFBool with DFMember.RefOwner =
        ctx.db.addMember(Dynamic(relVal, func, ctx.owner, ctx.meta)).asRefOwner[DFBool.Type, Modifier.Val]
      ret
    }

    object Unref {
      def unapply(arg : Dynamic)(
        implicit getSet: MemberGetSet
      ) : Option[(DFAny.Member, Func, DFOwner, DFMember.Tags)] =
        Some((arg.relValRef.get, arg.func, arg.ownerRef.get, arg.tags))
    }
    object IsNotEmpty {
      def apply(relVal : DFAny.Member)(implicit ctx : DFAny.Context) : DFBool = Dynamic(relVal, Func.IsNotEmpty)
      def unapply(arg : Dynamic)(
        implicit getSet: MemberGetSet
      ) : Option[(DFAny.Member, DFOwner, DFMember.Tags)] = arg match {
        case Unref(relVal, Func.IsNotEmpty, owner, tags) => Some((relVal, owner, tags))
        case _ => None
      }
    }
    object IsNotFull {
      def apply(relVal : DFAny.Member)(implicit ctx : DFAny.Context) : DFBool = Dynamic(relVal, Func.IsNotFull)
      def unapply(arg : Dynamic)(
        implicit getSet: MemberGetSet
      ) : Option[(DFAny.Member, DFOwner, DFMember.Tags)] = arg match {
        case Unref(relVal, Func.IsNotFull, owner, tags) => Some((relVal, owner, tags))
        case _ => None
      }
    }
    object IsNotStall {
      def apply(relVal : DFAny.Member)(implicit ctx : DFAny.Context) : DFBool = Dynamic(relVal, Func.IsNotStall)
      def unapply(arg : Dynamic)(
        implicit getSet: MemberGetSet
      ) : Option[(DFAny.Member, DFOwner, DFMember.Tags)] = arg match {
        case Unref(relVal, Func.IsNotStall, owner, tags) => Some((relVal, owner, tags))
        case _ => None
      }
    }
    object Consume {
      def apply(relVal : DFAny.Member)(implicit ctx : DFAny.Context) : DFMember = Dynamic(relVal, Func.Consume)
      def unapply(arg : Dynamic)(
        implicit getSet: MemberGetSet
      ) : Option[(DFAny.Member, DFOwner, DFMember.Tags)] = arg match {
        case Unref(relVal, Func.Consume, owner, tags) => Some((relVal, owner, tags))
        case _ => None
      }
    }
    object DontConsume {
      def apply(relVal : DFAny.Member)(implicit ctx : DFAny.Context) : DFMember = Dynamic(relVal, Func.DontConsume)
      def unapply(arg : Dynamic)(
        implicit getSet: MemberGetSet
      ) : Option[(DFAny.Member, DFOwner, DFMember.Tags)] = arg match {
        case Unref(relVal, Func.DontConsume, owner, tags) => Some((relVal, owner, tags))
        case _ => None
      }
    }
    object DontProduce {
      def apply(relVal : DFAny.Member)(implicit ctx : DFAny.Context) : DFMember = Dynamic(relVal, Func.DontProduce)
      def unapply(arg : Dynamic)(
        implicit getSet: MemberGetSet
      ) : Option[(DFAny.Member, DFOwner, DFMember.Tags)] = arg match {
        case Unref(relVal, Func.DontProduce, owner, tags) => Some((relVal, owner, tags))
        case _ => None
      }
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Extension Classes
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  type ConnOf[Type <: DFAny.Type] = Value[Type, Modifier]
  type VarOf[Type <: DFAny.Type] = Value[Type, Modifier.Assignable]
  implicit class VarOps[Type <: DFAny.Type](left : DFAny.VarOf[Type]) {
    def dontProduce()(implicit ctx : DFAny.Context) : Unit = Dynamic.DontProduce(left)
    final def isNotFull(implicit ctx : DFAny.Context) : DFBool = Dynamic.IsNotFull(left)
  }

  object In {
    def unapply(arg: DFAny.Member): Boolean = arg.modifier match {
      case Modifier.Port(IN) => true
      case _ => false
    }
  }
  object Out {
    def unapply(arg: DFAny.Member): Boolean = arg.modifier match {
      case Modifier.Port(OUT) => true
      case _ => false
    }
  }
  object Var {
    def unapply(arg: DFAny.Member): Boolean = arg.modifier match {
      case _ : Modifier.NewVar => true
      case _ => false
    }
  }

  type ConnectableOf[Type <: DFAny.Type] = Value[Type, Modifier.Connectable]
  final implicit class ConnectableOps[L](left : L)(implicit tcL : L => DFAny.Member) {
    protected implicit class ConnectionExtras[T](that : DFAny.Member) {
      def getPort(implicit ctx : DFNet.Context) : DFAny.Member = that match {
        case DFAny.Port.In() | DFAny.Port.Out() => that
        case alias : DFAny.Alias => alias.relValRef.get.getPort
        case _ => ???
      }
      def isConnectingExternally(implicit ctx : DFNet.Context) : Boolean = getPort.getOwnerDesign.getOwnerDesign == ctx.owner.getThisOrOwnerDesign
      def isConnectingInternally(implicit ctx : DFNet.Context) : Boolean = getPort.getOwnerDesign == ctx.owner.getThisOrOwnerDesign
    }
    private def connectPortInWithPortIn(left : DFAny.Member, right : DFAny.Member)(
      implicit ctx : DFNet.Context
    ) : (DFAny.Member, DFAny.Member) = {
      def throwConnectionError(msg : String) = throw new IllegalArgumentException(s"\n$msg\nAttempted connection: ${left.getFullName} <> ${right.getFullName} at ${ctx.owner.getFullName}")
      if (left isSameOwnerDesignAs right) throwConnectionError("Cannot connect two input ports of the same design.")
      //Connecting owner and child design input ports, while child port is left and owner port is right.
      else if ((left isOneLevelBelow right) && (left.isConnectingExternally)) (left, right)
      //Connecting owner and child design input ports, while child port is right and owner port is left.
      else if ((right isOneLevelBelow left) && (right.isConnectingExternally)) (right, left)
      else throwConnectionError("Unsupported connection")
    }
    private def connectPortOutWithPortOut(left : DFAny.Member, right : DFAny.Member)(
      implicit ctx : DFNet.Context
    ) : (DFAny.Member, DFAny.Member) = {
      def throwConnectionError(msg : String) = throw new IllegalArgumentException(s"\n$msg\nAttempted connection: ${left.getFullName} <> ${right.getFullName} at ${ctx.owner.getFullName}")
      if (left isSameOwnerDesignAs right) throwConnectionError("Cannot connect two output ports of the same design.")
      //Connecting owner and child design output ports, while child port is left and owner port is right.
      else if ((left isOneLevelBelow right) && (left.isConnectingExternally)) (right, left)
      //Connecting owner and child design output ports, while child port is right and owner port is left.
      else if ((right isOneLevelBelow left) && (right.isConnectingExternally)) (left, right)
      else throwConnectionError("Unsupported connection")
    }
    private def connectPortOutWithPortIn(out : DFAny.Member, in : DFAny.Member)(
      implicit ctx : DFNet.Context
    ) : (DFAny.Member, DFAny.Member) = {
      def throwConnectionError(msg : String) = throw new IllegalArgumentException(s"\n$msg\nAttempted connection: ${out.getFullName} <> ${in.getFullName} at ${ctx.owner.getFullName}")
      //Connecting input and output ports internally at the same design
      if ((out isSameOwnerDesignAs in) && out.isConnectingInternally) (out, in)
      //Connecting input and output ports of sibling designs
      else if ((out.getOwnerDesign isSameOwnerDesignAs in.getOwnerDesign) && out.isConnectingExternally) (in, out)
      else throwConnectionError("Unsupported connection")
    }
    private def connectVarWithPortIn(dfVar : DFAny.Member, in : DFAny.Member)(
      implicit ctx : DFNet.Context) : (DFAny.Member, DFAny.Member
      ) = {
      def throwConnectionError(msg : String) = throw new IllegalArgumentException(s"\n$msg\nAttempted connection: ${dfVar.getFullName} <> ${in.getFullName} at ${ctx.owner.getFullName}")
      //Connecting a value to an input port externally
      if ((in isOneLevelBelow dfVar) && (in.isConnectingExternally || in.isConnectingInternally)) (in, dfVar)
      //Connecting a an input port to a variable internally
      else if ((in isSameOwnerDesignAs dfVar) && (in.isConnectingInternally)) (dfVar, in)
      else throwConnectionError("Unsupported connection")
    }
    private def connectVarWithPortOut(dfVar : DFAny.Member, out : DFAny.Member)(
      implicit ctx : DFNet.Context
    ) : (DFAny.Member, DFAny.Member) = {
      def throwConnectionError(msg : String) = throw new IllegalArgumentException(s"\n$msg\nAttempted connection: ${dfVar.getFullName} <> ${out.getFullName} at ${ctx.owner.getFullName}")
      //Connecting a value to an output port internally
      if ((dfVar isSameOwnerDesignAs out) && (out.isConnectingInternally)) (out, dfVar)
      //Connecting a an output port to a variable externally
      else if ((out isOneLevelBelow dfVar) && (out.isConnectingExternally || out.isConnectingInternally)) (dfVar, out)
      else throwConnectionError("Unsupported connection")
    }
    private def connectValWithPortIn(dfVal : DFAny.Member, in : DFAny.Member)(
      implicit ctx : DFNet.Context
    ) : (DFAny.Member, DFAny.Member) = {
      def throwConnectionError(msg : String) = throw new IllegalArgumentException(s"\n$msg\nAttempted connection: ${dfVal.getFullName} <> ${in.getFullName} at ${ctx.owner.getFullName}")
      //Connecting a value to an input port externally
      if ((in isOneLevelBelow dfVal) && (in.isConnectingExternally || in.isConnectingInternally)) (in, dfVal)
      else if ((in isSameOwnerDesignAs dfVal) && (in.isConnectingInternally) && dfVal.isAnonymous) (in, dfVal)
      else throwConnectionError("Unsupported connection")
    }
    private def connectValWithPortOut(dfVal : DFAny.Member, out : DFAny.Member)(
      implicit ctx : DFNet.Context
    ) : (DFAny.Member, DFAny.Member) = {
      def throwConnectionError(msg : String) = throw new IllegalArgumentException(s"\n$msg\nAttempted connection: ${out.getFullName} <> ${dfVal.getFullName} at ${ctx.owner.getFullName}")
      //Connecting a value to an output port internally
      if ((dfVal isSameOwnerDesignAs out) && (out.isConnectingInternally)) (out, dfVal)
      else throwConnectionError("Unsupported connection")
    }
    private def connectVarWithVar(left : DFAny.Member, right : DFAny.Member)(
      implicit ctx : DFNet.Context
    ) : (DFAny.Member, DFAny.Member) = {
      def throwConnectionError(msg : String) = throw new IllegalArgumentException(s"\n$msg\nAttempted connection: ${left.getFullName} <> ${right.getFullName} at ${ctx.owner.getFullName}")
      (ctx.db.hasToConnectionFor(left), ctx.db.hasToConnectionFor(right)) match {
        case (true, false) => (right, left)
        case (false, true) => (left, right)
        case (true, true) => throwConnectionError("Both variables already have source connections")
        case (false, false) => throwConnectionError("Both variables do not have a source connection")
      }
    }

    protected[DFiant] def unidirConnect[R](right : R, lazyConnect : Boolean)(implicit ctx : DFNet.Context, tcR : R => DFAny.Member) : DFNet = {
      def throwConnectionError(msg : String) = throw new IllegalArgumentException(s"\n$msg\nAttempted connection: ${left.getFullName} <> ${right.getFullName}")
      val (toPort, from) : (DFAny.Member, DFAny.Member) = (tcL(left), tcR(right)) match {
        case (p1@In(),  p2@In())  => connectPortInWithPortIn(p1, p2)
        case (p1@Out(), p2@Out()) => connectPortOutWithPortOut(p1, p2)
        case (p1@Out(), p2@In())  => connectPortOutWithPortIn(p1, p2)
        case (p1@In(),  p2@Out()) => connectPortOutWithPortIn(p2, p1)
        case (p@In(),   v@Var())  => connectVarWithPortIn(v, p)
        case (v@Var(),  p@In())   => connectVarWithPortIn(v, p)
        case (p@Out(),  v@Var())  => connectVarWithPortOut(v, p)
        case (v@Var(),  p@Out())  => connectVarWithPortOut(v, p)
        case (v1@Var(), v2@Var()) => connectVarWithVar(v1, v2)
        case (p@In(),   v)        => connectValWithPortIn(v, p)
        case (v,        p@In())   => connectValWithPortIn(v, p)
        case (p@Out(),  v)        => connectValWithPortOut(v, p)
        case (v,        p@Out())  => connectValWithPortOut(v, p)
        case _                    => throwConnectionError(
          s"""Connection must be made between either of the following options:
             |* A port and a value
             |* Two ports
             |* Two vars where one variable already received an incoming connection""".stripMargin
        )
      }
      if (lazyConnect) DFNet.LazyConnection(toPort, from)
      else DFNet.Connection(toPort, from)
    }

    protected[DFiant] def connect[R](right : R)(implicit ctx : DFNet.Context, tcR : R => DFAny.Member) : DFNet =
      unidirConnect(right, lazyConnect = false)
    protected[DFiant] def `<LZ>`[R](right : R)(implicit ctx : DFNet.Context, tcR : R => DFAny.Member) : DFNet =
      unidirConnect(right, lazyConnect = true)
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Token
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  trait Token extends Product with Serializable {
    //maximum token value width
    val width : Int
    val bubbleMask : BitVector
    val valueBits : BitVector
    val dfType : DFAny.Type
    final def isBubble : Boolean = !(bubbleMask === BitVector.low(width))
    final def bits : DFBits.Token = DFBits.Token(valueBits, bubbleMask)
    final def bits(relBitHigh : Int, relBitLow : Int) : DFBits.Token =
      bitsWL(relBitHigh - relBitLow + 1, relBitLow)
    final def bit(relBit : Int) : DFBool.Token = {
      val outValueBits = valueBits.bit(relBit)
      val outBubbleMask = bubbleMask.bit(relBit)
      if (outBubbleMask) DFBool.Token.bubble(logical = false)
      else DFBool.Token(logical = false, outValueBits)
    }
    final def bitsWL[W](relWidth : TwoFace.Int[W], relBitLow : Int) : DFBits.TokenW[W] = {
      val outValueBits = valueBits.bitsWL(relWidth, relBitLow)
      val outBubbleMask = bubbleMask.bitsWL(relWidth, relBitLow)
      DFBits.Token(outValueBits, outBubbleMask).asInstanceOf[DFBits.TokenW[W]]
    }
    def ==(that : Token)(implicit bb : Bubble.Behaviour) : DFBool.Token
    final def !=(that : Token)(implicit bb : Bubble.Behaviour) : DFBool.Token = !(this == that)
    def codeString(implicit printer: CSPrinter) : String
  }
  type TokenT[Token <: DFAny.Token, Type <: DFAny.Type] = Token with DFAny.TypeTag[Type]
  object Token {
    trait Of[Type <: DFAny.Type, Value] extends Token {left =>
      val value : Option[Value]
      final lazy val (valueBits, bubbleMask) : (BitVector, BitVector) = value match {
        case Some(t) => (valueToBitVector(t), false.toBitVector(width))
        case None => (0.toBitVector(width), true.toBitVector(width))
      }
      final protected def mkTokenB(right : Of[Type, Value], f : (Value, Value) => Boolean) : DFBool.Token = {
        (left.value, right.value) match {
          case (Some(l), Some(r)) => DFBool.Token(logical = true, f(l, r))
          case _ => DFBool.Token.bubble(logical = true)
        }
      }
      final def == (right : Token)(implicit bb : Bubble.Behaviour) : DFBool.Token = right match {
        case of : Of[Type @unchecked, Value @unchecked] => mkTokenB(of, _ == _)
        case _ => ???
      }
      final def codeString(implicit printer : CSPrinter) : String = value match {
        case Some(t) => valueCodeString(t)
        case None => "?"
      }
      def valueToBitVector(value : Value) : BitVector
      def valueCodeString(value : Value)(implicit printer: CSPrinter) : String
    }
    sealed trait Exact
    sealed trait ToFit extends Exact
    sealed trait AsIs extends Exact
    object Exact extends ExactType[Exact]
    object ToFit extends ExactType[ToFit]
    object AsIs extends ExactType[AsIs]

    trait Frontend extends
      DFBits.Token.Frontend.Inherited with
      DFEnum.Token.Frontend.Inherited with
      DFBool.Token.Frontend.Inherited with
      DFDecimal.Token.Frontend.Inherited with
      DFVector.Token.Frontend.Inherited with
      DFStruct.Token.Frontend.Inherited

    trait BubbleOfToken[T <: Token] {
      def apply(t : T) : T
    }
    trait BubbleOfDFType[Type <: DFAny.Type] {
      def apply(t : Type) : Type#TToken
    }
    final implicit class TokenExt[T <: DFAny.Token](token : T) {
      def typeTag[Type <: DFAny.Type] : TokenT[T, Type] = token.@@[DFAny.TypeTag[Type]]
    }
    final implicit class TokenSeqInit[T <: Token](tokenSeq : Seq[T]) {
      def prevInit(step : Int) : Seq[T] = {
        val length = tokenSeq.length
        //No init at all, so invoking prev does not change anything (bubble tokens will be used)
        if ((length == 0) || (step == 0)) tokenSeq
        //The step is larger or equals to the init sequence, so only the last init token remains
        else if (length <= step) Seq(tokenSeq.last)
        //More tokens are available than the step size, so we drop the first, according to the step count
        else tokenSeq.drop(step)
      }
      def bits : Seq[DFBits.Token] =
        tokenSeq.map(t => t.bits)
      def bitsWL(relWidth : Int, relBitLow : Int) : Seq[DFBits.Token] =
        tokenSeq.map(t => t.bitsWL(relWidth, relBitLow))
      def codeString(implicit printer: CSPrinter) : String = tokenSeq.map(t => t.codeString).mkString("(", ", ", ")")
      //      def patternMatch(pattern : T#TPattern) : Seq[DFBool.Token] = TokenSeq(tokenSeq, pattern)((l, r) => l.patternMatch(r.asInstanceOf[l.TPattern]))
    }
  }

  object TokenSeq {
    def apply[O <: Token, T1 <: Token, T2 <: Token, T3 <: Token](t1 : Seq[T1], t2 : Seq[T2], t3 : Seq[T3])(op : (T1, T2, T3) => O) : Seq[O] =
      if (t1.isEmpty || t2.isEmpty || t3.isEmpty) Seq() else{
        val leftSeq = t1
        val rightSeq = t2
        val leftSeq2 = leftSeq.zipAll(rightSeq, leftSeq.last, rightSeq.last)
        val rightSeq2 = t3
        leftSeq2.zipAll(rightSeq2, leftSeq2.last, rightSeq2.last).map(t => op(t._1._1, t._1._2, t._2))
      }
    def apply[O <: Token, L <: Token, R <: Token](leftSeq : Seq[L], rightSeq : Seq[R])(op : (L, R) => O) : Seq[O] =
      if (leftSeq.isEmpty || rightSeq.isEmpty) Seq() else
        leftSeq.zipAll(rightSeq, leftSeq.last, rightSeq.last).map(t => op(t._1, t._2))
    def apply[O <: Token, L <: Token, R](leftSeq : Seq[L], rightConst : R)(op : (L, R) => O) : Seq[O] =
      leftSeq.map(t => op(t, rightConst))
    def apply[O <: Token, T <: Token](seq : Seq[T])(op : T => O) : Seq[O] =
      seq.map(t => op(t))
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Match Pattern
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  sealed trait Pattern {
    type TType <: Type
    type TValue
    def matches(value : TValue) : Boolean
    protected[DFiant] def matchCond(matchVal : DFAny.Of[TType])(
      implicit ctx : DFAny.Context
    ) : DFBool
    def codeString(implicit printer: CSPrinter) : String
  }
  object Pattern {
    sealed trait Of[P <: Of[P]] extends Pattern {
      def overlapsWith(pattern: P) : Boolean
    }
    abstract class OfIntervalSet[T <: Type, V, P <: OfIntervalSet[T, V, P]](val dfType : T, val patternSet : IntervalSet[V])(
      implicit codeStringOf: CodeStringOf[Interval[V]]
    ) extends Of[P] {
      type TType = T
      type TValue = V
      protected def matchCond(matchVal: DFAny.Of[T], interval : Interval[V])(
        implicit ctx: DFAny.Context
      ): DFBool
      final protected[DFiant] def matchCond(matchVal: DFAny.Of[T])(
        implicit ctx: DFAny.Context
      ): DFBool = {
        import DFDesign.Frontend._
        patternSet.toList.map(matchCond(matchVal, _)).reduce(_ || _)
      }

      override def equals(obj : Any) : Boolean = obj match {
        case that : OfIntervalSet[_, _ , _] => this.patternSet == that.patternSet
        case _ => false
      }
      final def matches(value : TValue) : Boolean = patternSet.containsPoint(value)
      final def overlapsWith(pattern: P) : Boolean = patternSet.intersect(pattern.patternSet).nonEmpty
      final def codeString(implicit printer: CSPrinter) : String = patternSet.map(t => codeStringOf(t)).mkString(", ")
    }
    abstract class OfSet[T <: Type, V, P <: OfSet[T, V, P]](val patternSet : Set[V])(
      implicit codeStringOf: CodeStringOf[V]
    ) extends Of[P] {
      type TType = T
      type TValue = V
      protected def matchCond(matchVal: DFAny.Of[T], value : V)(
        implicit ctx: DFAny.Context
      ): DFBool
      final protected[DFiant] def matchCond(matchVal: DFAny.Of[T])(
        implicit ctx: DFAny.Context
      ): DFBool = {
        import DFDesign.Frontend._
        patternSet.toList.map(matchCond(matchVal, _)).reduce(_ || _)
      }
      override def equals(obj : Any) : Boolean = obj match {
        case that : OfSet[_, _ , _] => this.patternSet == that.patternSet
        case _ => false
      }
      final def matches(value : TValue) : Boolean = patternSet.contains(value)
      final def overlapsWith(pattern: P) : Boolean = patternSet.intersect(pattern.patternSet).nonEmpty
      final def codeString(implicit printer: CSPrinter) : String = patternSet.map(t => codeStringOf(t)).mkString(", ")
    }
    trait Able[+R] {
      val right : R
    }

    trait Builder[LType <: Type, Able[+R] <: Pattern.Able[R]] {
      def apply[R](left : LType, right : Seq[Able[R]]) : left.TPattern
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Init
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  object Init {
    trait Able[LType <: DFAny.Type, -T] {
      def apply(lType : LType) : LType#TToken
    }
    object Able {
      implicit def fromBubble[LType <: DFAny.Type](bubble : Bubble) : Able[LType, Bubble] = t => t.getBubbleToken
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Op
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  object Op {
    trait Able[R]{val value : R}
    object Able {
      implicit def fromAble[R](able : Exact[R]) : R = able.value
    }
    trait Builder[-L, -R] extends HasOut {
      type Out <: DFAny
      def apply(left : L, rightR : R) : Out
    }
  }
  object `Op==,!=` {
    @scala.annotation.implicitNotFound("Dataflow variable of type ${L} does not support in/equality operation with the type ${R}")
    sealed trait Builder[-L, Op <: Func2.Op, -R] extends Op.Builder[L, R]{type Out = DFBool}
    @scala.annotation.implicitNotFound("Dataflow variable of type ${LType} does not support in/equality operation with the type ${RType}")
    trait Capable[LType <: DFAny.Type, RType <: DFAny.Type] {
      def unsafeCheck(leftType : LType, rightType : RType) : Unit
    }
    @scala.annotation.implicitNotFound("Dataflow variable of type ${VType} does not support in/equality operation with the constant type ${CType}")
    trait ConstCapable[VType <: DFAny.Type, CType <: DFAny.Type] {
      def unsafeCheck(varType : VType, constType : CType) : Unit
    }
    trait LowPriority {
      def create[L, LType <: DFAny.Type,  Op <: Func2.Op.Equality, R, RType <: DFAny.Type]
      (properLR : (L, R) => (DFAny.Of[LType], DFAny.Of[RType]))(
        implicit ctx : DFAny.Context, op : Op
      ) : Builder[L, Op, R] = new Builder[L, Op, R] {
        def apply(leftL : L, rightR : R) : DFBool = trydf {
          val (left, right) = properLR(leftL, rightR)
          val tokenFunc : (Token, Token) => DFBool.Token = op match {
            case Func2.Op.== => _ == _
            case Func2.Op.!= => _ != _
          }
          DFAny.Func2.forced(DFBool.Type(logical = true), left, op, right)(tokenFunc)
        }
      }
      implicit def __Type_eq_ConstToFit[VType <: DFAny.Type, Op <: Func2.Op.Equality, R](
        implicit
        ctx : DFAny.Context, op : Op,
        rConst : DFAny.Const.ToFit[VType, R],
      ) : Builder[DFAny.Of[VType], Op, R] =
        create[DFAny.Of[VType], VType, Op, R, VType] { (dfVal, constValue) =>
          val const = rConst(dfVal.dfType, constValue)
          (dfVal, const)
        }
      implicit def __ConstToFit_eq_Type[VType <: DFAny.Type, Op <: Func2.Op.Equality, L](
        implicit
        ctx : DFAny.Context, op : Op,
        lConst : DFAny.Const.ToFit[VType, L]
      ) : Builder[L, Op, DFAny.Of[VType]] =
        create[L, VType, Op, DFAny.Of[VType], VType] { (constValue, dfVal) =>
          val const = lConst(dfVal.dfType, constValue)
          (const, dfVal)
        }
    }
    object Builder extends LowPriority {
      implicit def __Type_eq_Type[LType <: DFAny.Type, Op <: Func2.Op.Equality, RType <: DFAny.Type](
        implicit ctx : DFAny.Context, op : Op, capable : Capable[LType, RType]
      ) : Builder[DFAny.Of[LType], Op, DFAny.Of[RType]] =
        create[DFAny.Of[LType], LType, Op, DFAny.Of[RType], RType]{(left, right) =>
          capable.unsafeCheck(left.dfType, right.dfType)
          (left, right)
        }

      implicit def __Type_eq_ConstAsIs[VType <: DFAny.Type, Op <: Func2.Op.Equality, R, CType <: DFAny.Type](
        implicit
        ctx : DFAny.Context, op : Op,
        rConst : DFAny.Const.AsIs.Aux[VType, R, CType],
        capable: ConstCapable[VType, CType]
      ) : Builder[DFAny.Of[VType], Op, R] =
        create[DFAny.Of[VType], VType, Op, R, CType] { (dfVal, constValue) =>
          val const = rConst(dfVal.dfType, constValue)
          capable.unsafeCheck(dfVal.dfType, const.dfType)
          (dfVal, const)
        }

      implicit def __ConstAsIs_eq_Type[VType <: DFAny.Type, Op <: Func2.Op.Equality, L, CType <: DFAny.Type](
        implicit
        ctx : DFAny.Context, op : Op,
        lConst : DFAny.Const.AsIs.Aux[VType, L, CType],
        capable: ConstCapable[VType, CType]
      ) : Builder[L, Op, DFAny.Of[VType]] =
        create[L, CType, Op, DFAny.Of[VType], VType] { (constValue, dfVal) =>
          val const = lConst(dfVal.dfType, constValue)
          capable.unsafeCheck(dfVal.dfType, const.dfType)
          (const, dfVal)
        }
    }
  }

  object `Op==` {
    type Builder[-L, -R] = `Op==,!=`.Builder[L, Func2.Op.==, R]
  }
  object `Op!=` {
    type Builder[-L, -R] = `Op==,!=`.Builder[L, Func2.Op.!=, R]
  }

  object `Op:=,<>` {
    @scala.annotation.implicitNotFound("Dataflow variable of type ${LType} does not support assignment/connect operation with the type ${R}")
    trait Builder[LType <: Type, -R] extends Op.Builder[LType, R]{type Out = Of[LType]}
    object Builder {
      implicit def __Type_ac_Type[LType <: Type](
        implicit ctx : DFNet.Context
      ) : Builder[LType, DFAny.Of[LType]] = (left, rightR) => {
        left.assignCheck(rightR)
        rightR
      }
      implicit def __Type_ac_Bubble[LType <: Type](
        implicit ctx : DFNet.Context
      ) : Builder[LType, Bubble] = (left, _) => {
        DFAny.Const(left, left.getBubbleToken)
      }
      implicit def __Type_ac_Const[LType <: Type, R](
        implicit
        rConst : DFAny.Const.ToFit[LType, R]
      ) : Builder[LType, R] = (left, rightValue) => rConst(left, rightValue)
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Create Companion object of DFXXX extenders of DFAny
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  trait Companion {
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Match Pattern
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////
    trait PatternCO {
      type Able[+R] <: DFAny.Pattern.Able[R]
      type Builder[LType <: DFAny.Type] <: DFAny.Pattern.Builder[LType, Able]
    }
    val Pattern : PatternCO
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
}
