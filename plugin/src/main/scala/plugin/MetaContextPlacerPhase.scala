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
 */
class MetaContextPlacerPhase(setting: Setting) extends CommonPhase:
  import tpd._

  val phaseName = "MetaContextPlacer"

  override val runsAfter = Set("TopAnnot")
  override val runsBefore = Set("FixInterpDFValPhase")
  // override val debugFilter: String => Boolean = _.contains("Playground.scala")
  var dfcArgStack = List.empty[Tree]
  var emptyDFCSym: TermSymbol = uninitialized
  var emptyNoEODFCSym: TermSymbol = uninitialized
  var dfcTpe: Type = uninitialized
  var dfSpecTpe: Type = uninitialized
  var hasClsMetaArgsTpe: TypeRef = uninitialized
  var clsMetaArgsTpe: TypeRef = uninitialized
  var topAnnotSym: ClassSymbol = uninitialized
  var appTpe: TypeRef = uninitialized
  var noTopAnnotIsRequired: TypeRef = uninitialized
  val defaultParamMap = mutable.Map.empty[ClassSymbol, Map[Int, Tree]]
  override def prepareForTypeDef(tree: TypeDef)(using Context): Context =
    val sym = tree.symbol
    tree.rhs match
      case template: Template if tree.hasDFC =>
        if (sym.is(Final) && !sym.isAnonymousClass)
          report.error("DFHDL classes cannot be final.", tree.srcPos)
        else if (sym.is(CaseClass))
          report.error("DFHDL classes cannot be case classes.", tree.srcPos)
        dfcArgStack = ContextArg.at(tree).get :: dfcArgStack
      case _ =>
    ctx

  private def clsMetaArgsOverrideDef(owner: Symbol, clsMetaArgsTree: Tree)(using
      Context
  ): Tree =
    val sym = newSymbol(
      owner,
      "__clsMetaArgs".toTermName,
      Override | Protected | Method | Touched,
      clsMetaArgsTpe
    )
    DefDef(sym, clsMetaArgsTree)
  end clsMetaArgsOverrideDef

  private def clsMetaArgsOverrideDef(owner: Symbol)(using Context): Tree =
    clsMetaArgsOverrideDef(owner, ref(requiredMethod("dfhdl.internals.ClsMetaArgs.empty")))
  private def genDesignBodyParams(
      body: List[Tree],
      paramList: List[Tree],
      defaults: Map[Int, Tree],
      dfcTree: Tree
  )(using
      Context
  ): (List[Tree], List[ValDef]) =
    val designParamMap = mutable.Map.empty[Symbol, Tree]
    val designParamGenValDefs: List[ValDef] = paramList.view.zipWithIndex.collect {
      case (v: ValDef, i) if v.dfValTpeOpt.nonEmpty =>
        // check and report error if the user did not apply a constant modifier
        // on a design parameter
        if (!v.tpt.tpe.isDFConst)
          report.error(
            "DFHDL design parameters must be constant values (use a `<> CONST` modifier).",
            v.tpt
          )
        val valDef = v.genDesignParamValDef(defaults.get(i), dfcTree)
        designParamMap += v.symbol -> ref(valDef.symbol)
        valDef
    }.toList
    (body.map(b => replaceArgs(b, designParamMap.toMap)), designParamGenValDefs)
  end genDesignBodyParams

  override def prepareForStats(trees: List[Tree])(using Context): Context =
    var explored: List[Tree] = trees
    while (explored.nonEmpty)
      explored match
        case (td @ TypeDef(tn, template: Template)) :: rest if td.hasDFC =>
          val clsSym = td.symbol.asClass
          val defaultMap = mutable.Map.empty[Int, Tree]
          rest match
            case (module: ValDef) :: (compSym @ TypeDef(_, compTemplate: Template)) :: _
                if compSym.symbol.companionClass == clsSym =>
              compTemplate.body.foreach {
                case dd @ DefDef(name = NameKinds.DefaultGetterName(n, i))
                    if dd.dfValTpeOpt.nonEmpty =>
                  defaultMap += i -> ref(module.symbol).select(dd.symbol)
                case _ =>
              }
              defaultParamMap += clsSym -> defaultMap.toMap
            case _ =>
          explored = rest
        case _ =>
          explored = explored.drop(1)
    end while
    ctx
  end prepareForStats

  override def transformTypeDef(tree: TypeDef)(using Context): TypeDef =
    tree.rhs match
      case template: Template =>
        var dfcArgOpt: Option[Tree] = None
        if (tree.hasDFC)
          dfcArgOpt = Some(dfcArgStack.head)
          dfcArgStack = dfcArgStack.drop(1)
        val clsTpe = tree.tpe
        val clsSym = clsTpe.classSymbol.asClass

        if (clsTpe <:< hasClsMetaArgsTpe && !clsSym.isAnonymousClass)
          val paramBody = template.body.takeWhile {
            case x: TypeDef                 => true
            case x: ValDef if x.rhs.isEmpty => true
            case _                          => false
          }
          val nonParamBody = template.body.drop(paramBody.length)
          val (updatedBody, designParamGenValDefs) = dfcArgOpt match
            case Some(dfcTree) =>
              val defaults = defaultParamMap.getOrElse(clsSym, Map.empty)
              genDesignBodyParams(nonParamBody, paramBody, defaults, dfcTree)(using
                ctx.withOwner(clsSym.primaryConstructor)
              )
            case None => (nonParamBody, Nil)
          val simpleArgs = paramBody.collect {
            case v: ValDef if v.dfValTpeOpt.isEmpty =>
              mkTuple(
                List(Literal(Constant(v.name.toString)), ref(v.symbol))
              )
          }
          val simpleArgsListMapTree =
            if (simpleArgs.isEmpty)
              ref(requiredMethod("scala.collection.immutable.ListMap.empty"))
                .appliedToTypes(List(defn.StringType, defn.AnyType))
            else
              ref(requiredModule("scala.collection.immutable.ListMap")).select(nme.apply)
                .appliedToTypes(List(defn.StringType, defn.AnyType))
                .appliedToVarargs(
                  simpleArgs,
                  TypeTree(
                    defn.Tuple2.typeRef.appliedTo(defn.StringType, defn.AnyType)
                  )
                )
          // TODO: The override does not seem to be actually used by the runtime,
          // probably because it's selected during the typer stage and needs to be
          // changed somehow to reference the new overridden tree symbol.
          // val clsMetaArgsTree = New(
          //   clsMetaArgsTpe,
          //   List(
          //     Literal(Constant(tree.name.toString)),
          //     tree.positionTree,
          //     mkOptionString(clsSym.docString),
          //     mkList(clsSym.staticAnnotations.map(_.tree)),
          //     simpleArgsListMapTree
          //   )
          // )
          // val clsMetaArgsDefTree =
          //   clsMetaArgsOverrideDef(clsSym.primaryConstructor, clsMetaArgsTree)
          val setClsNamePosTree =
            This(clsSym)
              .select("setClsNamePos".toTermName)
              .appliedToArgs(
                List(
                  Literal(Constant(tree.name.toString)),
                  tree.positionTree,
                  mkOptionString(clsSym.docString),
                  mkList(clsSym.staticAnnotations.map(a => dropProxies(a.tree))),
                  simpleArgsListMapTree
                )
              )
          val newTemplate =
            cpy.Template(template)(body =
              paramBody ++ List(setClsNamePosTree) ++ designParamGenValDefs ++ updatedBody
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
            tree.fun.symbol.isClassConstructor && tpe.isParameterless && !ctx.owner.isClassConstructor &&
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
                val uniqueName = NameKinds.UniqueName.fresh(s"arg_plugin".toTermName)
                val valDef = SyntheticValDef(uniqueName, a)
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

  override def prepareForUnit(tree: Tree)(using Context): Context =
    super.prepareForUnit(tree)
    emptyDFCSym = requiredMethod("dfhdl.core.DFC.empty")
    emptyNoEODFCSym = requiredMethod("dfhdl.core.DFC.emptyNoEO")
    dfcTpe = requiredClassRef("dfhdl.core.DFC")
    dfSpecTpe = requiredClassRef("dfhdl.DFSpec")
    hasClsMetaArgsTpe = requiredClassRef("dfhdl.internals.HasClsMetaArgs")
    clsMetaArgsTpe = requiredClassRef("dfhdl.internals.ClsMetaArgs")
    topAnnotSym = requiredClass("dfhdl.top")
    appTpe = requiredClassRef("dfhdl.app.DFApp")
    noTopAnnotIsRequired = requiredClassRef("dfhdl.internals.NoTopAnnotIsRequired")
    dfcArgStack = Nil
    defaultParamMap.clear()
    ctx
end MetaContextPlacerPhase
