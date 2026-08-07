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

/** The single home of DFHDL's user-facing diagnostic rewriting, applied by [[CustomReporter]] on
  * the real compilation and by [[PluginTestPhase]] on nested snippet compilations, so specs assert
  * on exactly what a user reads.
  */
final class DiagnosticRewriter(symbols: DFHDLSymbols.Cache):
  /** The frame of the diagnostic's inline position chain to report at. Dropping the outer chain
    * (see [[CustomReporter]]) is only sound when the innermost position is trustworthy, and a
    * diagnostic raised on a macro-synthesized tree is not: its innermost frame carries the span of
    * the quote inside the macro's own source paired with the CURRENT unit's source file, so the
    * rendered position lands past the unit's end (`Playground.scala:13:12843`-style). The chain is
    * walked innermost to outermost, keeping the first frame that belongs to the compiled unit
    * (`unitSource`: passed explicitly, since in a NESTED snippet compilation the chain extends past
    * the snippet's virtual source into the enclosing real unit, so the outermost frame does not
    * identify it) with a span that fits inside it. For every well-formed diagnostic the innermost
    * frame qualifies, so this changes nothing; only corrupt or library-positioned frames are
    * skipped.
    */
  def normalizedPos(pos: util.SourcePosition, unitSource: util.SourceFile): util.SourcePosition =
    val frames = Iterator
      .iterate(pos)(_.outer)
      .takeWhile(p => p != null && p.exists)
      .toList
    if (frames.isEmpty) pos
    else
      def sane(p: util.SourcePosition): Boolean =
        p.span.exists && p.span.end <= p.source.content().length
      frames.find(p => (p.source eq unitSource) && sane(p)).getOrElse(frames.last)
  end normalizedPos

  /** The identity of a diagnostic AS RENDERED: the same inline-expansion error re-raised at several
    * positions collapses onto one normalized position, so it must render once.
    */
  def dedupKey(dia: Diagnostic, unitSource: util.SourceFile)(using
      Context
  ): (String, Int, Int, Int, String) =
    val diaPos = normalizedPos(dia.pos, unitSource)
    val (spanStart, spanEnd) =
      if (diaPos.span.exists) (diaPos.span.start, diaPos.span.end) else (-1, -1)
    (diaPos.source.file.path, spanStart, spanEnd, dia.level, dia.msg.toString)

  /** The message to report in place of `base`. Every message is re-rendered through the DFHDL type
    * printer. A type mismatch whose REQUIRED side is a DFHDL value is additionally re-issued with
    * an EMPTY postscript, since the compiler's own trailing guidance is noise or worse there (the
    * transparent-inline note explains the Scala mechanics behind the DFHDL operators, and the
    * import suggestions, `InitValue.fromValue` and friends, never fix a DFHDL mismatch): a fresh
    * message rather than `mapMsg`, since `mapMsg` deliberately carries the original postscript, and
    * the postscript itself is protected so it cannot be filtered piecewise. The `-explain`
    * explanation is kept. `untpdRoot` is the compiled unit's parse tree, used to name the enclosing
    * call in [[reduceGuideRail]] (pass `untpd.EmptyTree` when unavailable).
    */
  def updatedMsg(base: Message, userPos: util.SourcePosition, untpdRoot: untpd.Tree)(using
      Context
  ): Message =
    // `toString` rather than `message`: it renders the message proper (without the postscript)
    // under the context the message captured, where the DFHDL type printer is live
    val rendered = base.toString
    val dropPostscript = base match
      case tm: TypeMismatchMsg =>
        val syms = symbols()
        syms.available && tm.expected.derivesFrom(syms.dfVal)
      case _ => false
    if (dropPostscript)
      val withGuideRail = rendered ++ reduceGuideRail(base, userPos, untpdRoot)
      new Message(base.errorId):
        val kind = base.kind
        def msg(using Context) = withGuideRail
        override def msgPostscript(using Context) = ""
        def explain(using Context) = base.explanation
        override def canExplain = base.canExplain
    else base.mapMsg(_ => rendered)
  end updatedMsg

  // The `(dfType, modifier args)` decomposition of a DFHDL value type, or None for anything else.
  private def dfValParts(tp: Type)(using Context): Option[(Type, List[Type])] =
    val syms = symbols()
    tp.dealias match
      case AppliedType(tycon, List(t, mod)) if tycon.typeSymbol == syms.dfVal =>
        mod.dealias match
          case AppliedType(modTycon, args @ List(_, _, _, _))
              if modTycon.typeSymbol == syms.modifier =>
            Some((t, args))
          case _ => None
      case _ => None

  private val foldFamily = Set(
    "reduce", "reduceLeft", "reduceRight", "reduceOption", "reduceLeftOption",
    "reduceRightOption", "fold", "foldLeft", "foldRight", "scan", "scanLeft", "scanRight"
  )

  // The simple name of the innermost call in `untpdRoot` one of whose arguments contains `pos`
  // (the typed tree does not exist yet at reporting time, but the parse tree does). A parent is
  // visited before its children, so the last match recorded is the innermost. Purely cosmetic,
  // so any failure to answer is just `None`.
  private def enclosingCallName(pos: util.SourcePosition, untpdRoot: untpd.Tree)(using
      Context
  ): Option[String] =
    try
      if (untpdRoot.isEmpty || !pos.span.exists) None
      else
        var found: Option[String] = None
        def nameOf(fun: untpd.Tree): Option[String] = fun match
          case untpd.Select(_, name) => Some(name.show)
          case untpd.Ident(name)     => Some(name.show)
          case untpd.TypeApply(f, _) => nameOf(f)
          case untpd.Apply(f, _)     => nameOf(f)
          case _                     => None
        val traverser = new untpd.UntypedTreeTraverser:
          def traverse(tree: untpd.Tree)(using Context): Unit =
            tree match
              case untpd.Apply(fun, args)
                  if args.exists(a => a.span.exists && a.span.contains(pos.span)) =>
                nameOf(fun).foreach(n => found = Some(n))
              case _ =>
            traverseChildren(tree)
        traverser.traverse(untpdRoot)
        found
      end if
    catch case scala.util.control.NonFatal(_) => None
  end enclosingCallName

  /** The guide rail for a plain computed value found where a declaration-modified value of the SAME
    * DFHDL type is required (`Found: Bits[Int] <> VAL` vs `Required: Bits[Int] <> IN`): the
    * signature of a `reduce`-style method that inferred its type parameter from port/variable slice
    * elements before the operator was typed, where pinning the type parameter to the plain value
    * type is the fix. When the enclosing call is identified as a known fold-family method the note
    * asserts and names it; otherwise it stays conditional. Empty for every other mismatch.
    */
  private def reduceGuideRail(
      base: Message,
      userPos: util.SourcePosition,
      untpdRoot: untpd.Tree
  )(using Context): String =
    base match
      case tm: TypeMismatch =>
        val syms = symbols()
        val hint =
          for
            (foundT, foundMod) <- dfValParts(tm.found)
            (expectedT, expectedMod) <- dfValParts(tm.expected)
            // found is a plain value (Any access), required is declaration-modified, and the
            // DFHDL type parts agree, so retyping the requirement as a plain value must succeed
            if foundMod.head.isRef(defn.AnyClass) && !expectedMod.head.isRef(defn.AnyClass) &&
              (foundT =:= expectedT)
          yield
            val plainMod = syms.modifier.typeRef.appliedTo(List.fill(4)(defn.AnyType))
            val plainVal = syms.dfVal.typeRef.appliedTo(List(expectedT, plainMod)).show
            enclosingCallName(userPos, untpdRoot).filter(foldFamily) match
              case Some(name) =>
                s"""|
                    |
                    |Note: `$name` inferred its type parameter from the declaration (port or
                    |variable) slice elements, so the operator must land back on the declaration
                    |type, and an operation result is a plain value that never can. Set the type
                    |parameter to the plain value type explicitly:
                    |
                    |  .$name[$plainVal](...)""".stripMargin
              case None =>
                s"""|
                    |
                    |Note: the required type belongs to a declaration (a port or a variable), and an
                    |operation result is a plain value that can never take its place. If this is the
                    |operator of a method like `reduce`, the method inferred its type parameter from
                    |the declaration slices before the operator was typed; set it to the plain value
                    |type explicitly:
                    |
                    |  .reduce[$plainVal](...)""".stripMargin
            end match
        hint.getOrElse("")
      case _ => ""
  end reduceGuideRail
end DiagnosticRewriter

/** Re-renders every reported diagnostic before passing it on, which is what puts DFHDL's own type
  * printer in front of the user (see [[DFHDLTypePrinter]]).
  *
  * The rendering swap is `Message.toString` rather than `Diagnostic.message`. They produce the same
  * text out of the same message, but by different routes: `message` renders under
  * `Message.inMessageContext`, which pins the printer to the compiler's own `Message.Printer` and
  * so never consults the one this phase installs, whereas `toString` renders under the context the
  * message captured, where that printer is live. Re-reporting also drops the diagnostic's outer
  * position, which suppresses inline-stack error printing.
  *
  * The rewriting itself (position normalization, dedup identity, postscript handling and the DFHDL
  * guide rails) lives in [[DiagnosticRewriter]], which the nested snippet compilations of
  * [[PluginTestPhase]] share, so `assertPluginError` specs assert on exactly what a user reads.
  * Re-reporting bypasses the original reporter's `UniqueMessagePositions` dedup (that dedup keys on
  * the positions the rewriter rewrites), so the rewriter's own dedup is applied in `isHidden`.
  */
class CustomReporter(
    val orig: Reporter,
    symbols: DFHDLSymbols.Cache
) extends Reporter:
  private val rewriter = DiagnosticRewriter(symbols)
  private val reported = collection.mutable.HashSet.empty[(String, Int, Int, Int, String)]
  override def flush()(using ctx: Context): Unit = orig.flush()
  // the compiled unit's parse tree, for naming the enclosing call in the guide rail; the
  // reporting context is the typing context, so its unit is the one holding the error
  private def untpdRootFor(pos: util.SourcePosition)(using Context): untpd.Tree =
    try
      val unit = ctx.compilationUnit
      if ((unit ne null) && (pos.source eq unit.source)) unit.untpdTree else untpd.EmptyTree
    catch
      case scala.util.control.NonFatal(_) => untpd.EmptyTree
  // the dedup lives in `isHidden` rather than `doReport` so a swallowed duplicate is also
  // never counted, keeping the "N errors found" summary consistent with what is rendered
  // (the same reason the compiler's own dedup, `UniqueMessagePositions`, works at this hook)
  override def isHidden(dia: Diagnostic)(using Context): Boolean =
    super.isHidden(dia) ||
      dia.level >= interfaces.Diagnostic.WARNING &&
      !reported.add(rewriter.dedupKey(dia, ctx.source))
  override def doReport(dia: Diagnostic)(using ctx: Context): Unit =
    val userPos = rewriter.normalizedPos(dia.pos, ctx.source)
    val diaPos = userPos.copy(outer = null) // disable inline stack error printing
    val newMsg = rewriter.updatedMsg(dia.msg, userPos, untpdRootFor(userPos))
    orig.doReport(Diagnostic(newMsg, diaPos, dia.level))
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
  *     Classes extending `Interface` are excluded, since they are never entry points and must not
  *     receive `@top`.
  *
  * It also owns two run-wide reporting hooks, installed from `initContext`: the
  * [[DFHDLTypePrinter]] that names DFHDL types the way a DFHDL user writes them, and the
  * [[CustomReporter]] that re-renders every reported diagnostic through that printer.
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
      // The annotation is fully qualified as `_root_.dfhdl.top`: an unqualified `top`
      // resolves to the annotated class itself when the class is named `top` (a common
      // Verilog top-module convention), yielding a baffling "Cyclic reference involving
      // class top" error (#458).
      untpd.Apply(
        untpd.Select(
          untpd.New(
            untpd.Select(
              untpd.Select(untpd.Ident(nme.ROOTPKG), "dfhdl".toTermName),
              "top".toTypeName
            )
          ),
          nme.CONSTRUCTOR
        ),
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
    end shouldAddTop

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

    // Approach A: ensure every `@top` class has a companion object *before* the
    // typer runs, so the namer establishes real companion linkage. The
    // entry-point `main` is later injected into this companion by TopAnnotPhase
    // (a same-named module created post-typer is NOT recognized as a companion,
    // so the backend would emit a clashing mirror class).
    //
    // This stands in as the *primary* companion the user would have written, so
    // it must NOT be `Synthetic`: when the class has default constructor args,
    // the desugarer emits its own `Synthetic` companion for the default getters,
    // and `Namer.mergeCompanionDefs` only merges the two when exactly one of
    // them is synthetic (two synthetics => "X is already defined" error). It
    // carries the class definition's full span (`cdef.span`), matching dotty's
    // own synthetic-companion span convention in `Desugar.companionDefs`.
    private def mkEmptyCompanion(td: TypeDef)(using Context): ModuleDef =
      val tmpl = untpd.Template(untpd.emptyConstructor, Nil, Nil, untpd.EmptyValDef, Nil)
      untpd.ModuleDef(td.name.toTermName, tmpl).withSpan(td.span)
    end mkEmptyCompanion

    private def addTopCompanions(stats: List[Tree])(using Context): List[Tree] =
      val moduleNames = stats.collect { case md: ModuleDef => md.name.toString }.toSet
      val newCompanions = stats.collect {
        case td @ TypeDef(name, _: Template)
            if !td.mods.is(Trait) && hasTopAnnot(td.mods) &&
              !moduleNames.contains(name.toString) =>
          mkEmptyCompanion(td)
      }
      if (newCompanions.isEmpty) stats else stats ++ newCompanions
    end addTopCompanions

    override def transformStats(trees: List[Tree], exprOwner: Symbol)(using
        Context
    ): List[Tree] =
      val transformed = super.transformStats(trees, exprOwner)
      // Only synthesize companions in class/object/package scopes (where auto-
      // `@top` is also permitted) — never inside method bodies, lambdas, blocks.
      if (validOwnerScope) addTopCompanions(transformed) else transformed
    end transformStats

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
  // Applies this phase's parse-tree rewrites to a standalone parsed tree, so nested snippet
  // compilations (PluginTestPhase) get the same parse-level fidelity as regular units. The
  // auto-@top rewrite is deliberately skipped: it never applies inside block snippets.
  def rewriteParsed(tree: Tree)(using Context): Tree =
    `fixXand<>Precedence`.transform(`fix<>andOpPrecedence`.transform(tree))

  // The symbols the DFHDL type printer matches against, cached per run. The cache belongs to
  // this phase instance rather than to a global, so compilers running concurrently in one JVM
  // never see each other's symbols (see DFHDLSymbols.Cache).
  private val printerSymbols = DFHDLSymbols.Cache()

  // installs the DFHDL type printer, so every type the compiler reports on its own initiative
  // (a type mismatch, a missing member, an IDE hover) names DFHDL types the way a DFHDL user
  // writes them; see DFHDLTypePrinter. `-P:dfhdl.plugin:disableCustomPrinter` leaves both hooks
  // uninstalled, which is how a diagnostic gets read in the compiler's own vocabulary while
  // working on the DSL.
  override def initContext(ctx: FreshContext): Unit =
    if (!setting.disableCustomPrinter)
      ctx.setPrinterFn(printerCtx =>
        DFHDLTypePrinter(printerCtx, printerSymbols()(using printerCtx))
      )
      val typerState = ctx.typerState.setReporter(new CustomReporter(ctx.reporter, printerSymbols))
      ctx.setTyperState(typerState)
  end initContext

  override def runOn(units: List[CompilationUnit])(using Context): List[CompilationUnit] =
    val parsed = super.runOn(units)
    // `dfhdl.top` lives in the `lib` subproject — only apply the auto-@top
    // rewrite when it's reachable on the classpath of this compilation.
    val topAvailable = getClassIfDefined("dfhdl.top").exists
    parsed.foreach { cu =>
      debugFlag = cu.source.file.path.contains("Playground.scala")
      cu.untpdTree = rewriteParsed(cu.untpdTree)
      if (topAvailable)
        cu.untpdTree = `autoTopAnnot`.transform(cu.untpdTree)
    }
    parsed
  end runOn
end PreTyperPhase
