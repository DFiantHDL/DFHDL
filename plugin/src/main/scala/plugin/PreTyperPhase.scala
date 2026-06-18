package dfhdl.plugin

import dotty.tools.dotc._

import plugins._

import core._
import Contexts._
import Symbols._
import Flags._
import SymDenotations._

import Decorators._
import ast.Trees._
import ast.untpd
import StdNames.nme
import Names._
import Constants.Constant
import Types._
import scala.language.implicitConversions
import collection.mutable
import annotation.tailrec
import reporting.*

// not used, but can be potentially useful for modified the reported compiler errors
class CustomReporter(
    val orig: Reporter
) extends Reporter:
  override def flush()(using ctx: Context): Unit = orig.flush()
  override def doReport(dia: Diagnostic)(using ctx: Context): Unit =
    val updatedMsg = dia.msg.toString
    val diaPos = dia.pos.copy(outer = null) // disable inline stack error printing
    val updatedDia = Diagnostic(dia.msg.mapMsg(x => updatedMsg), diaPos, dia.level)
    orig.doReport(updatedDia)
  end doReport
end CustomReporter

/** This is a pre-typer phase that does very minor things:
  *   - change infix operator precedence of type signature: `a X b <> c` to be `(a X b) <> c`
  *   - change infix operator precedence of terms: `a <> b op c` to be `a <> (b op c)` and `a op b
  *     <> c` to be `(a op b) <> c`, where op is `|`, `||`, `&`, `&&`, `^`, or a comparison operator
  *   - change infix operator precedence of terms: `a := b match {...}` to be `a := (b match {...})`
  *     and `a <> b match {...}` to be `a <> (b match {...})`
  *   - change process{} to process.forever{}
  *   - auto-add `@top` annotation to concrete classes that look like DFHDL designs (extend
  *     EDDesign/RTDesign/DFDesign, have `type <> CONST` parameters, or use `<>` in their body),
  *     provided `import dfhdl.*` is in lexical scope and no `@top` annotation is already present.
  *     Classes extending `Interface` are excluded, since they are never entry points and
  *     must not receive `@top`.
  */
class PreTyperPhase(setting: Setting) extends CommonPhase:
  import untpd.*

  val phaseName = "PreTyper"

  override val runsAfter = Set("parser")
  override val runsBefore = Set("typer")
  private var debugFlag = false
  // override to prevent from running redundant MiniPhase transformation
  // that can cause compiler errors
  override def run(using Context): Unit = {}

  def debug2(str: => Any*): Unit =
    if (debugFlag) println(str.mkString(", "))

  val opSet = Set("|", "||", "&", "&&", "^", "<<", ">>", "==", "!=", "<", ">", "<=", ">=")
  private val `fix<>andOpPrecedence` = new UntypedTreeMap:
    object InfixOpArgsChange:
      def unapply(tree: InfixOp)(using Context): Option[(Tree, Ident, Tree)] =
        tree match
          case InfixOp(InfixOpArgsChange(a, Ident(conn), b), Ident(op), c)
              if opSet.contains(op.toString) =>
            Some(a, Ident(conn), InfixOp(b, Ident(op), c))
          case InfixOp(a, Ident(op), InfixOpArgsChange(b, Ident(conn), c))
              if opSet.contains(op.toString) =>
            Some(InfixOp(a, Ident(op), b), Ident(conn), c)
          case InfixOp(a, Ident(op), InfixOp(b, Ident(conn), c))
              if conn.toString == "<>" && opSet.contains(op.toString) =>
            Some(InfixOp(a, Ident(op), b), Ident(conn), c)
          case InfixOp(InfixOp(a, Ident(conn), b), Ident(op), c)
              if conn.toString == "<>" && opSet.contains(op.toString) =>
            Some(a, Ident(conn), InfixOp(b, Ident(op), c))
          case _ =>
            None
    end InfixOpArgsChange
    object InfixOpChange:
      def unapply(tree: InfixOp)(using Context): Option[InfixOp] =
        tree match
          case InfixOpArgsChange(a, Ident(conn), b) => Some(InfixOp(a, Ident(conn), Parens(b)))
          case _                                    =>
            None
    end InfixOpChange
    object MatchAssignOpChange:
      def unapply(tree: Match)(using Context): Option[InfixOp] =
        tree match
          case Match(InfixOp(a, Ident(op), b), cases)
              if op.toString == ":=" || op.toString == "<>" =>
            Some(InfixOp(a, Ident(op), Parens(Match(b, cases))))
          case _ =>
            None
    object ProcessChange:
      def unapply(tree: Tree)(using Context): Option[Tree] =
        tree match
          case Apply(Ident(process), List(ofTree)) if process.toString == "process" =>
            Some(Apply(Select(Ident(process), "forever".toTermName), List(ofTree)))
          case ValDef(name, tpt, ProcessChange(rhs)) =>
            Some(ValDef(name, tpt, rhs))
          case _ => None
    override def transformBlock(blk: Block)(using Context): Block =
      super.transformBlock(blk) match
        // a connection/assignment could be in return expression position of a Unit-typed block
        case Block(stats, InfixOpChange(expr))       => Block(stats, expr)
        case Block(stats, MatchAssignOpChange(expr)) => Block(stats, expr)
        case Block(stats, ProcessChange(expr))       => Block(stats, expr)
        case blk                                     => blk
    override def transformStats(trees: List[Tree], exprOwner: Symbol)(using Context): List[Tree] =
      super.transformStats(trees, exprOwner).map:
        // only handling pure statements that begin as an infix
        case InfixOpChange(tree)       => tree
        case MatchAssignOpChange(tree) => tree
        // change process{} to process.forever{}
        case ProcessChange(tree) => tree
        case tree                => tree
    override def transform(tree: Tree)(using Context): Tree =
      super.transform(tree) match
        // a connection could be in return position of a DFHDL Unit definition (if no block is used)
        case tree @ DefDef(preRhs = InfixOpChange(rhs)) =>
          cpy.DefDef(tree)(rhs = rhs)
        case t => t
      end match
    end transform

  private val `autoTopAnnot` = new UntypedTreeMap:
    private var dfhdlImported: Boolean = false
    // True while traversing a subtree whose innermost enclosing owner is a class, object,
    // or package (i.e. a context in which a class definition is directly owned by a
    // class/object — the requirement enforced by the `@top` AnnotatedWith macro).
    // Flipped to false when entering a method body, lambda, or block expression.
    private var validOwnerScope: Boolean = true

    private def rightmostName(tree: Tree): Option[String] =
      tree match
        case Apply(fn, _) => rightmostName(fn)
        case Select(_, n) => Some(n.toString)
        case Ident(n)     => Some(n.toString)
        case New(tpt)     => rightmostName(tpt)
        case _            => None

    private def isTopAnnot(tree: Tree): Boolean =
      tree match
        case Apply(Select(New(tpt), ctor), _) if ctor == nme.CONSTRUCTOR =>
          rightmostName(tpt).contains("top")
        case Select(New(tpt), ctor) if ctor == nme.CONSTRUCTOR =>
          rightmostName(tpt).contains("top")
        case New(tpt) => rightmostName(tpt).contains("top")
        case _        => false

    private val designParentNames = Set("EDDesign", "RTDesign", "DFDesign")
    private val interfaceParentNames = Set("Interface")

    private def hasDesignParent(parents: List[Tree]): Boolean =
      parents.exists(p => rightmostName(p).exists(designParentNames))
    private def hasInterfaceParent(parents: List[Tree]): Boolean =
      parents.exists(p => rightmostName(p).exists(interfaceParentNames))

    private def isConstParamTpt(tpt: Tree): Boolean =
      tpt match
        case InfixOp(_, Ident(op), Ident(mod)) =>
          op.toString == "<>" && mod.toString == "CONST"
        case _ => false

    private def hasConstParam(paramss: List[ParamClause]): Boolean =
      paramss.flatten.exists {
        case vd: ValDef => isConstParamTpt(vd.tpt)
        case _          => false
      }

    // TopAnnotPhase's main-entry-point synthesis requires every constructor param to be
    // either defaulted or typed as `T <> CONST` (so the default can be synthesized). If
    // any param fails both tests, auto-adding `@top` would produce a confusing downstream
    // error — so we skip these classes and let users opt in manually with `@top`.
    private def allParamsTopCompatible(paramss: List[ParamClause])(using Context): Boolean =
      paramss.flatten.forall {
        case vd: ValDef => !vd.rhs.isEmpty || isConstParamTpt(vd.tpt)
        case _          => true
      }

    private def bodyUsesConnect(body: List[Tree])(using Context): Boolean =
      // Look for `<>` in two top-level-statement shapes:
      //  1. a val/var RHS (`val p = Bit <> IN`) — recurse into the RHS but stop at
      //     nested defs/lambdas/blocks/templates so `<>` buried inside munit
      //     `test("..."):` blocks or inner class bodies doesn't qualify the outer
      //     class as a design.
      //  2. a bare `<>` statement in the class body (`Vcc <> fpga.bank0`) — matched
      //     non-recursively (the tree itself must be an `InfixOp` with `<>`).
      // Over-matching here is now cheap: `@top(true)` is silently skipped by
      // TopAnnotPhase on non-Design classes, so false positives don't cause errors.
      val acc = new UntypedTreeAccumulator[Boolean]:
        def apply(x: Boolean, tree: Tree)(using Context): Boolean =
          if (x) true
          else tree match
            case InfixOp(_, Ident(op), _) if op.toString == "<>"               => true
            case _: Template | _: DefDef | _: Function | _: Block | _: TypeDef => x
            case _                                                             => foldOver(x, tree)
      body.exists {
        case vd: ValDef if !vd.rhs.isEmpty                   => acc(false, vd.rhs)
        case InfixOp(_, Ident(op), _) if op.toString == "<>" => true
        case _                                               => false
      }
    end bodyUsesConnect

    private def hasDfhdlWildcardImport(stats: List[Tree]): Boolean =
      stats.exists {
        case Import(expr, selectors) =>
          rightmostName(expr).contains("dfhdl") && selectors.exists {
            case ImportSelector(Ident(name), _, _) => name == nme.WILDCARD
          }
        case _ => false
      }

    private def mkTopAnnot(span: util.Spans.Span)(using Context): Tree =
      // Inject `@top(true)` rather than bare `@top`: the explicit-`true` form is the
      // lenient variant — TopAnnotPhase silently skips entry-point generation when the
      // annotated class turns out not to be a Design, whereas bare `@top` is strict
      // and would surface a compile error on a false positive.
      untpd.Apply(
        untpd.Select(untpd.New(untpd.Ident("top".toTypeName)), nme.CONSTRUCTOR),
        List(untpd.Literal(Constant(true)))
      ).withSpan(span)

    private def hasTopAnnot(mods: Modifiers): Boolean =
      mods.annotations.exists(isTopAnnot)

    private def shouldAddTop(td: TypeDef, tmpl: Template)(using Context): Boolean =
      val m = td.mods
      !m.is(Abstract) &&
      !m.is(Trait) &&
      !m.is(Case) &&
      !m.is(Enum) &&
      !hasTopAnnot(m) &&
      // interfaces are never entry points, so never auto-`@top` them (even when they
      // carry `<> CONST` params or use `<>` in their body for port/view declarations)
      !hasInterfaceParent(tmpl.parents) &&
      tmpl.constr.paramss.length <= 1 &&
      allParamsTopCompatible(tmpl.constr.paramss) &&
      (hasDesignParent(tmpl.parents) ||
        hasConstParam(tmpl.constr.paramss) ||
        bodyUsesConnect(tmpl.body))

    private inline def withScope[A](stats: List[Tree])(body: => A): A =
      val prev = dfhdlImported
      if (!dfhdlImported && hasDfhdlWildcardImport(stats)) dfhdlImported = true
      try body
      finally dfhdlImported = prev

    private inline def withInvalidScope[A](body: => A): A =
      val prev = validOwnerScope
      validOwnerScope = false
      try body
      finally validOwnerScope = prev

    override def transform(tree: Tree)(using Context): Tree =
      tree match
        case pkg @ PackageDef(_, stats) =>
          withScope(stats)(super.transform(pkg))
        case md @ ModuleDef(_, tmpl: Template) =>
          withScope(tmpl.body)(super.transform(md))
        case td @ TypeDef(_, _: Template) =>
          val canAnnotate = dfhdlImported && validOwnerScope
          // While descending into this class's own body, its nested classes ARE directly
          // owned by a class (this one), so re-enable validOwnerScope for the recursion.
          val prev = validOwnerScope
          validOwnerScope = true
          val transformed =
            try super.transform(td).asInstanceOf[TypeDef]
            finally validOwnerScope = prev
          transformed.rhs match
            case newTmpl: Template if canAnnotate && shouldAddTop(transformed, newTmpl) =>
              transformed.withMods(
                transformed.mods.withAddedAnnotation(mkTopAnnot(transformed.nameSpan))
              )
            case _ => transformed
        case _: DefDef | _: Function | _: Block =>
          withInvalidScope(super.transform(tree))
        case _ =>
          super.transform(tree)
    end transform
  end `autoTopAnnot`

  private val `fixXand<>Precedence` = new UntypedTreeMap:
    object InfixOpChange:
      def unapply(tree: InfixOp)(using Context): Option[InfixOp] =
        tree match
          case InfixOp(a, Ident(x), InfixOp(b, Ident(conn), c))
              if x.toString == "X" && conn.toString == "<>" =>
            Some(InfixOp(Parens(InfixOp(a, Ident(x), b)), Ident(conn), c))
          case _ => None
    object FullSelectGivenName:
      def unapply(tree: Select)(using Context): Option[String] =
        tree match
          case Select(Ident(options), name) if options.toString == "options" =>
            Some(s"options_${name}")
          case Select(FullSelectGivenName(prev), name) => Some(s"${prev}_$name")
          case _                                       => None
    override def transform(tree: Tree)(using Context): Tree =
      super.transform(tree) match
        case tree @ InfixOpChange(rhs) => rhs
        // workaround https://github.com/scala/scala3/issues/21406
        case tree @ ValDef(name, select: Select, _) if name.isEmpty && tree.mods.is(Given) =>
          select match
            case FullSelectGivenName(updateName) => cpy.ValDef(tree)(name = updateName.toTermName)
            case _                               => tree
        case t =>
          t
      end match
    end transform
  object DFType:
    def unapply(arg: Type)(using Context): Option[(String, List[Type])] =
      arg.simple match
        case AppliedType(dfTypeCore, List(n, argsTp))
            if dfTypeCore.typeSymbol == requiredClass("dfhdl.core.DFType") =>
          val nameStr = n.typeSymbol.name.toString
          argsTp match
            case AppliedType(_, args) => Some(nameStr, args)
            case _                    => Some(nameStr, Nil)
        case _ => None
  end DFType
  object DFBool:
    def unapply(arg: Type)(using Context): Boolean =
      arg match
        case DFType("DFBool$", Nil) => true
        case _                      => false
  object DFBit:
    def unapply(arg: Type)(using Context): Boolean =
      arg match
        case DFType("DFBit$", Nil) => true
        case _                     => false
  object DFBits:
    def unapply(arg: Type)(using Context): Option[Type] =
      arg match
        case DFType("DFBits", w :: Nil) => Some(w)
        case _                          => None
  object DFDecimal:
    def unapply(arg: Type)(using Context): Option[(Type, Type, Type)] =
      arg match
        // ignoring the fourth native argument, since it's not needed for matching
        case DFType("DFDecimal", s :: w :: f :: _ :: Nil) => Some(s, w, f)
        case _                                            => None
  object DFXInt:
    def unapply(arg: Type)(using Context): Option[(Boolean, Type)] =
      arg match
        case DFDecimal(
              ConstantType(Constant(sign: Boolean)),
              widthTpe,
              ConstantType(Constant(fractionWidth: Int))
            ) if fractionWidth == 0 =>
          Some(sign, widthTpe)
        case _ => None
  object DFUInt:
    def unapply(arg: Type)(using Context): Option[Type] =
      arg match
        case DFXInt(sign, widthTpe) if !sign => Some(widthTpe)
        case _                               => None
  object DFSInt:
    def unapply(arg: Type)(using Context): Option[Type] =
      arg match
        case DFXInt(sign, widthTpe) if sign => Some(widthTpe)
        case _                              => None
  object DFEnum:
    def unapply(arg: Type)(using Context): Option[Type] =
      arg match
        case DFType("DFEnum", e :: Nil) => Some(e)
        case _                          => None
  object DFStruct:
    def unapply(arg: Type)(using Context): Option[Type] =
      arg match
        case DFType("DFStruct", t :: Nil) => Some(t)
        case _                            => None

  object DFVal:
    private def stripAndType(tpeOpt: Option[Type])(using Context): Option[Type] =
      tpeOpt.map(tpe =>
        tpe.simple match
          case AndType(t1, _) => t1
          case _              => tpe
      )
    def unapply(arg: Type)(using Context): Option[Type] =
      val dfValClsRef = requiredClassRef("dfhdl.core.DFVal")
      val ret = arg.simple match
        case AppliedType(t, List(dfType, _)) if t <:< dfValClsRef =>
          Some(dfType)
        case AppliedType(t, List(arg, mod))
            if t.typeSymbol.name.toString == "<>" &&
              (mod <:< requiredClassRef("dfhdl.VAL") || mod <:< requiredClassRef("dfhdl.DFRET")) =>
          arg match
            case dfType @ DFType(_, _) => Some(dfType)
            case _                     => None
        case _ =>
          None
      stripAndType(ret)
    end unapply
  end DFVal

  // not used, but can be potentially useful for modified the reported compiler errors
  override def initContext(ctx: FreshContext): Unit =
    import dotty.tools.dotc.printing.*
    import dotty.tools.dotc.printing.Texts.Text
    def foo(ctx: Context): Printer =
      new RefinedPrinter(ctx):
        override def toText(tp: Type): Text =
          tp match
            case DFVal(dfType) =>
              val dfTypeText: Text = dfType match
                case DFBool()           => "Boolean"
                case DFBit()            => "Bit"
                case DFBits(w)          => s"Bits[${w.show}]"
                case DFUInt(w)          => s"UInt[${w.show}]"
                case DFSInt(w)          => s"SInt[${w.show}]"
                case DFDecimal(s, w, f) => s"Decimal[${s.show}, ${w.show}, ${f.show}]"
                case DFEnum(e)          => s"Enum[${e.show}]"
                case DFStruct(t)        => s"Struct[${t.show}]"
                case _                  => super.toText(tp)
              dfTypeText ~ " <> Val"
            case _ => super.toText(tp)
    ctx.setPrinterFn(foo)
    val typerState = ctx.typerState.setReporter(new CustomReporter(ctx.reporter))
    ctx.setTyperState(typerState)
  end initContext

  override def runOn(units: List[CompilationUnit])(using Context): List[CompilationUnit] =
    val parsed = super.runOn(units)
    // `dfhdl.top` lives in the `lib` subproject — only apply the auto-@top
    // rewrite when it's reachable on the classpath of this compilation.
    val topAvailable = getClassIfDefined("dfhdl.top").exists
    parsed.foreach { cu =>
      debugFlag = cu.source.file.path.contains("Playground.scala")
      cu.untpdTree = `fix<>andOpPrecedence`.transform(cu.untpdTree)
      cu.untpdTree = `fixXand<>Precedence`.transform(cu.untpdTree)
      if (topAvailable)
        cu.untpdTree = `autoTopAnnot`.transform(cu.untpdTree)
    }
    parsed
  end runOn
end PreTyperPhase
