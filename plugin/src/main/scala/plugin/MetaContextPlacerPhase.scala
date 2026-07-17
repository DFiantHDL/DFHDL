package dfhdl.plugin

import dotty.tools.dotc.*
import plugins.*
import core.*
import Contexts.*
import Symbols.*
import Flags.*
import SymDenotations.*
import Decorators.*
import ast.Trees.*
import ast.{tpd, untpd, TreeTypeMap}
import StdNames.nme
import Names.{Designator, *}
import Constants.Constant
import Types.*
import DenotTransformers.IdentityDenotTransformer

import scala.language.implicitConversions
import scala.compiletime.uninitialized
import collection.mutable
import annotation.tailrec
import dotty.tools.dotc.ast.Trees.Alternative

/*
  This phase overrides the `__dfc` def of DFHDL classes to propagate the DFC
  from an encapsulating class or def to its DFHDL class instance. If a class
  is instantiated regularly the instance is transformed into an anonymous
  class instance with the override, otherwise all is required is to add the
  additional override to an existing anonymous DFHDL class instance.
  Additionally, it transforms basic val x = y to val x = dfhdl.core.r__For_Plugin.identVal(y) if y is a DFVal
 */
class MetaContextPlacerPhase(setting: Setting) extends CapturePhase, IdentityDenotTransformer:
  import tpd._

  val phaseName = "MetaContextPlacer"

  override val runsAfter = Set("TopAnnot")
  override val runsBefore = Set("inlinedPositions")
  // We enter new (non-private) members into existing classes (e.g. the injected
  // overrides), so this phase must declare that it changes class members —
  // otherwise `enteredAfter(this)` asserts, and later phases (notably Mixin)
  // would not see the new members.
  override def changesMembers: Boolean = true
  // override val debugFilter: String => Boolean = _.contains("Playground.scala")
  var dfcArgStack = List.empty[Tree]
  var emptyDFCSym: TermSymbol = uninitialized
  var emptyNoEODFCSym: TermSymbol = uninitialized
  var dfcTpe: Type = uninitialized
  var dfSpecTpe: Type = uninitialized
  var hasClsMetaTpe: TypeRef = uninitialized
  var hasClsArgsTpe: TypeRef = uninitialized
  var designTpe: TypeRef = uninitialized
  var metaTpe: TypeRef = uninitialized
  var interfaceTpe: TypeRef = uninitialized
  var topAnnotSym: ClassSymbol = uninitialized
  var appTpe: TypeRef = uninitialized
  var noTopAnnotIsRequired: TypeRef = uninitialized
  var listMapEmptySym: TermSymbol = uninitialized
  var listMapSym: TermSymbol = uninitialized
  var dfhdlDFValIdentSym: TermSymbol = uninitialized
  var clsAppliedArgsSym: TermSymbol = uninitialized
  val defaultParamMap = mutable.Map.empty[ClassSymbol, Map[Int, Tree]]
  override def prepareForTypeDef(tree: TypeDef)(using Context): Context =
    val sym = tree.symbol
    tree.rhs match
      case template: Template if tree.hasDFC =>
        if (sym.is(Final) && !sym.isAnonymousClass)
          report.error("DFHDL classes cannot be final.", tree.srcPos)
        else if (sym.is(CaseClass))
          report.error("DFHDL classes cannot be case classes.", tree.srcPos)
        // Reject user-written anonymous interface instances (e.g. `new MyIfc() {}`).
        // An interface must be a named class so it has a stable identity (its
        // `interfaceRef` design block). The plugin's own instance anon-classes are
        // created in the transform pass (`transformApply`), which this prepare hook
        // never re-traverses, so every anon interface class seen here is user-written.
        else if (sym.isAnonymousClass && sym.typeRef <:< interfaceTpe)
          report.error(
            s"Cannot create an anonymous Interface class instance.\nInstantiate the class without a body (e.g. just `${sym.typeRef.parents.head.typeSymbol.name}()`)",
            tree.srcPos
          )
        dfcArgStack = ContextArg.at(tree).get :: dfcArgStack
      case _ =>
    end match
    ctx
  end prepareForTypeDef

  private def genContainerBodyParams(
      body: List[Tree],
      paramList: List[Tree],
      defaults: Map[Int, Tree],
      dfcTree: Tree
  )(using
      Context
  ): (List[Tree], List[ValDef]) =
    val paramMap = mutable.Map.empty[Symbol, Tree]
    val paramGenValDefs: List[ValDef] = paramList.view.zipWithIndex.collect {
      case (v: ValDef, i) if v.dfValTpeOpt.nonEmpty =>
        // check and report error if the user did not apply a constant modifier
        // on a design/interface parameter
        if (!v.tpt.tpe.isDFConst)
          report.error(
            "DFHDL design/interface parameters must be constant values (use a `<> CONST` modifier).",
            v.tpt
          )
        val valDef = v.genContainerParamValDef(defaults.get(i), dfcTree)
        paramMap += v.symbol -> ref(valDef.symbol)
        valDef
    }.toList
    (body.map(b => replaceArgs(b, paramMap.toMap)), paramGenValDefs)
  end genContainerBodyParams

  override def prepareForStats(trees: List[Tree])(using Context): Context =
    var explored: List[Tree] = trees
    object CompanionExtractor:
      def unapply(trees: List[Tree])
          : Option[(clsSym: ClassSymbol, module: ValDef, compTemplate: Template)] =
        trees match
          case (td @ TypeDef(tn, template: Template)) :: rest if td.hasDFC =>
            val clsSym = td.symbol.asClass
            var explore = rest
            var ret: Option[(clsSym: ClassSymbol, module: ValDef, compTemplate: Template)] = None
            while (explore.nonEmpty && ret.isEmpty)
              explore match
                case (module: ValDef) :: (compSym @ TypeDef(_, compTemplate: Template)) :: _
                    if compSym.symbol.companionClass == clsSym =>
                  ret = Some((clsSym, module, compTemplate))
                case _ => explore = explore.tail
            ret
          case _ => None
    end CompanionExtractor
    while (explored.nonEmpty)
      explored match
        case CompanionExtractor(clsSym, module, compTemplate) =>
          val defaultMap = mutable.Map.empty[Int, Tree]
          compTemplate.body.foreach {
            case dd @ DefDef(name = NameKinds.DefaultGetterName(n, i))
                if dd.dfValTpeOpt.nonEmpty =>
              defaultMap += i -> ref(module.symbol).select(dd.symbol)
            case _ =>
          }
          defaultParamMap += clsSym -> defaultMap.toMap
        case _ =>
      explored = explored.drop(1)
    end while
    ctx
  end prepareForStats

  // Build the
  //   override protected def __clsAppliedArgs: List[(String, ir.DFVal)] =
  //     r__For_Plugin.clsAppliedArgs(List(("name", param), ...))
  // injected into a DFHDL class (`HasClsArgs`) that declares `<> CONST` constructor
  // parameters — the applied parameter values at the instantiation site, used to construct
  // the design instance's `paramMap` at design end. A simple override suffices (no super
  // chaining): base-class parameters are recovered from their creation entries in the design
  // context (see `Design.Inst.collectParamEntries`).
  private def clsAppliedArgsOverrideDef(
      tree: TypeDef,
      clsSym: ClassSymbol,
      constParams: List[ValDef]
  )(using Context): DefDef =
    val superSym = clsSym.requiredMethod("__clsAppliedArgs".toTermName)
    val sym = newSymbol(
      clsSym,
      "__clsAppliedArgs".toTermName,
      (superSym.flags & (Protected | Method)) | Override | Touched,
      superSym.info,
      coord = tree.span
    ).enteredAfter(this)
    val ownArgs = mkList(
      constParams.map(v =>
        mkTuple(List(Literal(Constant(v.name.toString.nameCheck(v))), ref(v.symbol)))
      )
    )
    DefDef(sym, ref(clsAppliedArgsSym).appliedTo(ownArgs))
  end clsAppliedArgsOverrideDef

  // Build the
  //   override protected def __clsScalaArgs: List[Any] =
  //     List[Any](<plain Scala ctor params>, <plain Scala template captures>) ::: super.__clsScalaArgs
  // injected into a design class that has plain Scala constructor parameters or plain
  // Scala template captures. These values may legitimately shape the elaborated
  // structure, so they join the design load key (the class-design counterpart of a
  // method's `scalaArgs`). Each class in the inheritance chain prepends its own
  // contribution (like `__clsMeta`), so base-class captures are covered as well;
  // base-class constructor arguments need no entry of their own since they derive from
  // the leaf's (keyed) arguments and captures through code the key's `dclMeta` already
  // identifies.
  private def clsScalaArgsOverrideDef(
      tree: TypeDef,
      clsSym: ClassSymbol,
      ownArgs: List[Tree]
  )(using Context): DefDef =
    val superSym = clsSym.requiredMethod("__clsScalaArgs".toTermName)
    val sym = newSymbol(
      clsSym,
      "__clsScalaArgs".toTermName,
      (superSym.flags & (Protected | Method)) | Override | Touched,
      superSym.info,
      coord = tree.span
    ).enteredAfter(this)
    val ownList = mkList(ownArgs, Some(defn.AnyType))
    val superScalaArgs = Super(This(clsSym), StdNames.tpnme.EMPTY).select(superSym)
    val chain =
      superScalaArgs.select(":::".toTermName).appliedToType(defn.AnyType).appliedTo(ownList)
    DefDef(sym, chain)
  end clsScalaArgsOverrideDef

  // Build the
  //   override protected def __clsMeta: List[ir.Meta] =
  //     r__For_Plugin.metaGen(...) :: super.__clsMeta
  // injected into a DFHDL class. Each class in the inheritance chain prepends
  // its own meta, so the leaf's `__clsMeta` yields the full chain
  // (most-derived first). It is the declarative source of truth for class
  // metadata; each container builds its design block directly from this chain at
  // creation (`initOwner`), with no mutation.
  //
  // For the override to actually take effect via virtual dispatch (rather than
  // leaving the inherited default and letting Mixin emit a competing forwarder),
  // the symbol must (a) be owned by the class, (b) copy the inherited symbol's
  // flags/info for an exact signature match, and (c) be entered into the class's
  // decls before later phases (Mixin) run — hence `enteredAfter(this)`, enabled
  // by this phase being an `IdentityDenotTransformer` with `changesMembers`.
  private def clsMetaOverrideDef(tree: TypeDef, clsSym: ClassSymbol)(using Context): DefDef =
    // resolve the inherited (super) `__clsMeta` before entering our override
    val superSym = clsSym.requiredMethod("__clsMeta".toTermName)
    val sym = newSymbol(
      clsSym,
      "__clsMeta".toTermName,
      (superSym.flags & (Protected | Method)) | Override | Touched,
      superSym.info,
      coord = tree.span
    ).enteredAfter(this)
    val newMetaTree =
      ref(metaGenSym).appliedToArgs(
        List(
          mkOptionString(Some(clsSym.getFinalName())),
          tree.positionTree,
          mkOptionString(clsSym.docString),
          mkList(clsSym.staticAnnotations.map(a => reownLocalDefs(dropProxies(a.tree), sym)))
        )
      )
    // metaGen(...) :: super.__clsMeta   (i.e. super.__clsMeta.::(metaGen(...)))
    val superClsMeta = Super(This(clsSym), StdNames.tpnme.EMPTY).select(superSym)
    val chain =
      superClsMeta.select("::".toTermName).appliedToType(metaTpe).appliedTo(newMetaTree)
    DefDef(sym, chain)
  end clsMetaOverrideDef

  override def transformTypeDef(tree: TypeDef)(using Context): TypeDef =
    tree.rhs match
      case template: Template =>
        var dfcArgOpt: Option[Tree] = None
        if (tree.hasDFC)
          dfcArgOpt = Some(dfcArgStack.head)
          dfcArgStack = dfcArgStack.drop(1)
        val clsTpe = tree.tpe
        val clsSym = clsTpe.classSymbol.asClass

        if (clsTpe <:< hasClsMetaTpe && !clsSym.isAnonymousClass && !clsSym.flags.is(Trait))
          val paramBody = template.body.takeWhile {
            case x: TypeDef                 => true
            case x: ValDef if x.rhs.isEmpty => true
            case _                          => false
          }
          val nonParamBody = template.body.drop(paramBody.length)
          // only `HasClsArgs` classes (designs/interfaces) turn `<> CONST` constructor
          // parameters into design-parameter members — other `HasClsMeta` classes (e.g.
          // platform resources) may carry DFHDL-value parameters that must stay untouched
          val hasClsArgs = clsTpe <:< hasClsArgsTpe
          val (updatedBody, containerParamGenValDefs) = dfcArgOpt match
            case Some(dfcTree) if hasClsArgs =>
              val defaults = defaultParamMap.getOrElse(clsSym, Map.empty)
              genContainerBodyParams(nonParamBody, paramBody, defaults, dfcTree)(using
                ctx.withOwner(clsSym.primaryConstructor)
              )
            case _ => (nonParamBody, Nil)
          val clsMetaDef = clsMetaOverrideDef(tree, clsSym)
          // expose the class's applied `<> CONST` parameters (if any) through
          // `__clsAppliedArgs`
          val clsAppliedArgsDefOpt =
            val constParams =
              if (hasClsArgs)
                paramBody.collect { case v: ValDef if v.dfValTpeOpt.nonEmpty => v }
              else Nil
            if (constParams.nonEmpty)
              Some(clsAppliedArgsOverrideDef(tree, clsSym, constParams))
            else None
          // design classes key their plain Scala constructor parameters and template
          // captures through `__clsScalaArgs` (consumed by the design load gate)
          val clsScalaArgsDefOpt =
            if (clsTpe <:< designTpe)
              val scalaParams = paramBody.collect {
                case v: ValDef if v.dfValTpeOpt.isEmpty && !v.tpt.tpe.isMetaContext => v
              }
              val scalaCaptures = discoverClsCaptures(clsSym, template).scalaCaptures
              val ownArgs = scalaParams.map(v => ref(v.symbol)) ++ scalaCaptures.map(_._2)
              if (ownArgs.nonEmpty) Some(clsScalaArgsOverrideDef(tree, clsSym, ownArgs))
              else None
            else None
          val newTemplate =
            cpy.Template(template)(body =
              paramBody ++ List(clsMetaDef) ++ clsAppliedArgsDefOpt ++ clsScalaArgsDefOpt ++
                containerParamGenValDefs ++ updatedBody
            )
          cpy.TypeDef(tree)(rhs = newTemplate)
        else tree
        end if
      case _ =>
        tree
    end match
  end transformTypeDef

  override def prepareForDefDef(tree: DefDef)(using Context): Context =
    ContextArg.at(tree) match
      case Some(arg) =>
        dfcArgStack = arg :: dfcArgStack
      case _ =>
    ctx

  override def transformDefDef(tree: DefDef)(using Context): DefDef =
    ContextArg.at(tree) match
      case Some(arg) =>
        dfcArgStack = dfcArgStack.drop(1)
      case _ =>
    tree

  private def dfcOverrideDef(owner: Symbol, treeSrcPos: util.SrcPos)(using Context): Tree =
    val sym =
      newSymbol(owner, "__dfc".toTermName, Override | Protected | Method | Touched, dfcTpe)
    // getting DFC context from the stack or need to generate an empty one
    // with elaboration options found in the @top annotation
    val dfcArg = dfcArgStack.headOption.getOrElse {
      owner.getAnnotation(topAnnotSym).map(a => dropProxies(a.tree)) match
        // found top annotation
        case Some(Apply(Apply(Apply(_, _), _), topElaborationOptionsTree :: _)) =>
          ref(emptyDFCSym).appliedTo(topElaborationOptionsTree)
        // no top
        case _ =>
          var currentOwner = owner.owner
          while (currentOwner != NoSymbol && !(currentOwner.typeRef <:< noTopAnnotIsRequired))
            currentOwner = currentOwner.owner
          // no top, but if has an owner that extends `NoTopAnnotIsRequired`,
          // we generate new context with default elaboration options
          if (currentOwner.typeRef <:< noTopAnnotIsRequired) ref(emptyNoEODFCSym)
          else
            report.error(
              "Missing `@top` annotation for this design to be instantiated as a top-level design.",
              treeSrcPos
            )
            EmptyTree
    }
    DefDef(sym, dfcArg)
  end dfcOverrideDef

  override def transformApply(tree: Apply)(using Context): Tree =
    val tpe = tree.tpe
    tree match
      case Apply(Select(New(Ident(n)), _), _) if n == StdNames.tpnme.ANON_CLASS => tree
      case _
          if (
            tree.fun.symbol.isClassConstructor && tpe.isParameterless &&
              !ctx.owner.isClassConstructor &&
              !ctx.owner.isClassConstructor && tpe.typeConstructor <:< hasDFCTpe
          ) =>
        val cls = newNormalizedClassSymbol(
          ctx.owner,
          StdNames.tpnme.ANON_CLASS,
          Synthetic | Final,
          List(tpe),
          coord = tree.symbol.coord
        )
        cls.addAnnotations(tpe.typeSymbol.annotations)
        val constr = newConstructor(cls, Synthetic, Nil, Nil).entered
        var valDefs: List[ValDef] = Nil
        // naming the arguments before extending the tree as as parent because
        // otherwise ownership and references need to change.
        def nameArgs(tree: Tree): Tree =
          tree match
            case Apply(fun, args) =>
              val updatedArgs = args.map { a =>
                val strippedNamedArg = a match
                  case NamedArg(_, arg) => arg
                  case arg              => arg
                val uniqueName = NameKinds.UniqueName.fresh(s"arg_plugin".toTermName)
                val valDef = SyntheticValDef(uniqueName, strippedNamedArg)
                valDefs = valDef :: valDefs
                ref(valDef.symbol)
              }
              Apply(nameArgs(fun), updatedArgs)
            case _ => tree
        val parent = nameArgs(tree)
        val od = dfcOverrideDef(cls, tree.srcPos)
        val cdef = ClassDefWithParents(cls, DefDef(constr), List(parent), List(od))
        Block(
          valDefs.reverse :+ cdef,
          Typed(New(Ident(cdef.namedType)).select(constr).appliedToNone, TypeTree(tpe))
        )
      case _ => tree
    end match
  end transformApply
  override def transformBlock(tree: Block)(using Context): tpd.Tree =
    tree match
      case Block(
            List(td @ TypeDef(tn, template: Template)),
            Typed(apply @ Apply(fun, _), _)
          ) if tree.tpe.typeConstructor <:< hasDFCTpe =>
        val hasDFCOverride = template.body.exists {
          case dd: DefDef if dd.name.toString == "__dfc" => true
          case _                                         => false
        }
        if (hasDFCOverride) tree
        else
          val od = dfcOverrideDef(td.symbol, tree.srcPos)
          val updatedTemplate = cpy.Template(template)(body = od :: template.body)
          val updatedTypeDef = cpy.TypeDef(td)(rhs = updatedTemplate)
          cpy.Block(tree)(stats = List(updatedTypeDef), expr = tree.expr)
      case _ =>
        tree
  // Any DFVal member of an interface (port or const parameter) must be
  // access-restricted so it is only reachable through a view's `.VIEW`.
  override def prepareForValDef(tree: ValDef)(using Context): Context =
    val sym = tree.symbol
    if (
      sym.exists && sym.owner.isClass && sym.owner.typeRef <:< interfaceTpe &&
      tree.dfValTpeOpt.nonEmpty && !sym.isOneOf(Protected | Private)
    )
      report.error(
        """|Interface ports and parameters must be declared `protected`.
           |They are internal to the interface; expose ports through a view and
           |access them via `<instance>.<view>.VIEW`.""".stripMargin,
        tree.srcPos
      )
    ctx

  // transform basic val x = y to val x = dfhdl.core.r__For_Plugin.identVal(y) if y is a DFVal
  override def transformValDef(tree: ValDef)(using Context): ValDef =
    object DFValIdent:
      def unapply(tree: Tree)(using Context): Option[Tree] =
        tree match
          case ident @ Ident(name)
              if !ident.symbol.isOneOf(InlineProxy | Case) && !name.toString.contains("$") =>
            Some(tree)
          case Select(DFValIdent(_), _) => Some(tree)
          case This(DFValIdent(_))      => Some(tree)
          case _                        => None
    end DFValIdent
    def skipName(name: String): Boolean =
      name.contains("$") || name.startsWith("___")
    tree.rhs match
      case DFValIdent(rhs)
          if !tree.symbol.flags.isOneOf(InlineProxy | Case) && !skipName(tree.name.toString) &&
            tree.tpt.tpe.dfValTpeOpt.nonEmpty =>
        val dfc = dfcArgStack.headOption.getOrElse(ref(emptyNoEODFCSym))
        val updatedRHS =
          ref(dfhdlDFValIdentSym)
            .appliedToType(rhs.tpe.widen)
            .appliedTo(rhs)
            .appliedTo(dfc)
        cpy.ValDef(tree)(rhs = updatedRHS)
      case _ => tree
    end match
  end transformValDef

  override def prepareForUnit(tree: Tree)(using Context): Context =
    super.prepareForUnit(tree)
    emptyDFCSym = requiredMethod("dfhdl.core.DFC.empty")
    emptyNoEODFCSym = requiredMethod("dfhdl.core.DFC.emptyNoEO")
    dfcTpe = requiredClassRef("dfhdl.core.DFC")
    dfSpecTpe = requiredClassRef("dfhdl.DFSpec")
    hasClsMetaTpe = requiredClassRef("dfhdl.core.HasClsMeta")
    hasClsArgsTpe = requiredClassRef("dfhdl.core.HasClsArgs")
    designTpe = requiredClassRef("dfhdl.core.Design")
    metaTpe = requiredClassRef("dfhdl.compiler.ir.Meta")
    interfaceTpe = requiredClassRef("dfhdl.core.Interface")
    topAnnotSym = requiredClass("dfhdl.top")
    appTpe = requiredClassRef("dfhdl.app.DFApp")
    noTopAnnotIsRequired = requiredClassRef("dfhdl.internals.NoTopAnnotIsRequired")
    listMapEmptySym = requiredMethod("scala.collection.immutable.ListMap.empty")
    listMapSym = requiredModule("scala.collection.immutable.ListMap")
    dfhdlDFValIdentSym = requiredMethod("dfhdl.core.r__For_Plugin.identVal")
    clsAppliedArgsSym = requiredMethod("dfhdl.core.r__For_Plugin.clsAppliedArgs")
    dfcArgStack = Nil
    defaultParamMap.clear()
    ctx
  end prepareForUnit
end MetaContextPlacerPhase
