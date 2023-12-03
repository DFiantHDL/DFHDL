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
import collection.mutable
import annotation.tailrec
import dotty.tools.dotc.ast.Trees.Alternative

/*
  This phase override the `__dfc` def of DFHDL classes to propagate the DFC
  from an encapsulating class or def to its DFHDL class instance. If a class
  is instantiated regularly the instance is transformed into an anonymous
  class instance with the override, otherwise all is required is to add the
  additional override to an existing anonymous DFHDL class instance.
 */
class MetaContextPlacerPhase(setting: Setting) extends CommonPhase:
  import tpd._

  val phaseName = "MetaContextPlacer"

  override val runsAfter = Set("typer")
  override val runsBefore = Set("FixInterpDFValPhase")
  // override val debugFilter: String => Boolean = _.contains("PrioEncSpec.scala")
  var dfcSymStack = List.empty[Tree]
  var emptyDFCSym: TermSymbol = _
  var dfcTpe: Type = _
  var dfSpecTpe: Type = _

  extension (tree: TypeDef)
    def hasDFC(using Context): Boolean =
      (tree.tpe <:< hasDFCTpe) // && (dfSpecTpe == NoType || !(tree.tpe <:< dfSpecTpe))
  override def prepareForTypeDef(tree: TypeDef)(using Context): Context =
    val sym = tree.symbol
    tree.rhs match
      case template: Template if tree.hasDFC =>
        if (sym.is(Final) && !sym.isAnonymousClass)
          report.error("DFHDL classes cannot be final.", tree.srcPos)
        dfcSymStack = This(sym.asClass).select("dfc".toTermName) :: dfcSymStack
      case _ =>
    ctx

  override def transformTypeDef(tree: TypeDef)(using Context): TypeDef =
    val sym = tree.symbol
    tree.rhs match
      case template: Template if tree.hasDFC =>
        dfcSymStack = dfcSymStack.drop(1)
      case _ =>
    tree

  override def prepareForDefDef(tree: DefDef)(using Context): Context =
    tree match
      case ContextArg(arg) =>
        dfcSymStack = arg :: dfcSymStack
      case _ =>
    ctx

  override def transformDefDef(tree: DefDef)(using Context): DefDef =
    tree match
      case ContextArg(arg) =>
        dfcSymStack = dfcSymStack.drop(1)
      case _ =>
    tree

  private def dfcOverrideDef(owner: Symbol)(using Context): Tree =
    val sym =
      newSymbol(owner, "__dfc".toTermName, Override | Protected | Method | Touched, dfcTpe)
    val dfcSym = dfcSymStack.headOption.getOrElse(ref(emptyDFCSym))
    DefDef(sym, dfcSym)

  override def transformApply(tree: Apply)(using Context): Tree =
    val tpe = tree.tpe
    tree match
      case Apply(Select(New(Ident(n)), _), _) if n == StdNames.tpnme.ANON_CLASS => tree
      case _
          if (
            tree.fun.symbol.isClassConstructor && tpe.isParameterless && !ctx.owner.owner.isAnonymousClass &&
              !ctx.owner.isClassConstructor && tpe.typeConstructor <:< hasDFCTpe
          ) =>
        val cls = newNormalizedClassSymbol(
          ctx.owner,
          StdNames.tpnme.ANON_CLASS,
          Synthetic | Final,
          List(tpe),
          coord = tree.symbol.coord
        )
        val constr = newConstructor(cls, Synthetic, Nil, Nil).entered
        val encClass = ctx.owner.enclosingClass
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
        val od = dfcOverrideDef(cls)
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
          val od = dfcOverrideDef(td.symbol)
          val updatedTemplate = cpy.Template(template)(body = od :: template.body)
          val updatedTypeDef = cpy.TypeDef(td)(rhs = updatedTemplate)
          cpy.Block(tree)(stats = List(updatedTypeDef), expr = tree.expr)
      case _ =>
        tree

  override def prepareForUnit(tree: Tree)(using Context): Context =
    super.prepareForUnit(tree)
    emptyDFCSym = requiredMethod("dfhdl.core.DFC.empty")
    dfcTpe = requiredClassRef("dfhdl.core.DFC")
    dfSpecTpe = requiredClassRef("dfhdl.DFSpec")
    dfcSymStack = Nil
    ctx
end MetaContextPlacerPhase
