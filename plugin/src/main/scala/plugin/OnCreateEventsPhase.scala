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
import ast.{tpd, untpd}
import StdNames.nme
import Names.*
import Types.*
import Constants.Constant

import annotation.tailrec
import scala.language.implicitConversions
import collection.mutable

class OnCreateEventsPhase(setting: Setting) extends CommonPhase:
  import tpd._

  val phaseName = "OnCreateEvents"
//  override val debugFilter: String => Boolean = _.contains("DFDesignSpec.scala")

  override val runsAfter = Set("CustomControlPhase")
  override val runsBefore = Set(transform.FirstTransform.name)

  val ignore = mutable.Set.empty[Tree]
  var onCreateEventsTpe: TypeRef = _
  var hasNamePosTpe: TypeRef = _
  var clsStack = List.empty[TypeDef]
  override def prepareForTypeDef(tree: TypeDef)(using Context): Context =
    tree.rhs match
      case _: Template =>
        clsStack = tree :: clsStack
      case _ =>
    ctx

  override def transformTypeDef(tree: TypeDef)(using Context): Tree =
    tree.rhs match
      case template: Template =>
        clsStack = clsStack.drop(1)
        val clsTpe = tree.tpe
        val clsSym = clsTpe.typeSymbol
        if (clsTpe <:< hasNamePosTpe && !clsSym.isAnonymousClass)
          val args =
            template.constr.paramss.flatten.collect { case v: ValDef =>
              mkTuple(
                List(Literal(Constant(v.name.toString)), untpd.Ident(v.name).withType(v.tpe))
              )
            }

          val listMapTree =
            if (args.isEmpty)
              ref(requiredMethod("scala.collection.immutable.ListMap.empty"))
                .appliedToTypes(List(defn.StringType, defn.AnyType))
            else
              ref(requiredModule("scala.collection.immutable.ListMap")).select(nme.apply)
                .appliedToTypes(List(defn.StringType, defn.AnyType))
                .appliedToVarargs(
                  args,
                  TypeTree(
                    AppliedType(
                      requiredClassRef("scala.Tuple2"),
                      List(defn.StringType, defn.AnyType)
                    )
                  )
                )

          val setClsNamePosTree =
            This(clsSym.asClass)
              .select("setClsNamePos".toTermName)
              .appliedToArgs(
                List(
                  Literal(Constant(tree.name.toString)),
                  tree.positionTree,
                  mkOptionString(clsSym.docString),
                  mkList(clsSym.annotations.collect {
                    case a if a.tree.tpe <:< defn.StaticAnnotationClass.typeRef => a.tree
                  }),
                  listMapTree
                )
              )
          val newTemplate = cpy.Template(template)(
            template.constr,
            template.parents,
            template.derived,
            template.self,
            setClsNamePosTree :: template.body
          )
          cpy.TypeDef(tree)(tree.name, newTemplate)
        else if (clsTpe <:< onCreateEventsTpe && clsSym.isAnonymousClass)
          val onCreateStartLateTree =
            This(clsSym.asClass)
              .select("onCreateStartLate".toTermName)
          val newTemplate = cpy.Template(template)(
            template.constr,
            template.parents,
            template.derived,
            template.self,
            onCreateStartLateTree :: template.body
          )
          cpy.TypeDef(tree)(tree.name, newTemplate)
        else tree
        end if
      case _ =>
        tree
    end match
  end transformTypeDef

  private object OnCreateEventsInstance:
    def apply(tree: ValDef)(using Context): Tree =
      val clsSym = tree.tpe.classSymbol.asClass
      Select(This(clsStack.head.tpe.classSymbol.asClass), tree.name)
        .select(clsSym.requiredMethod("onCreate"))
    end apply
    def apply(clsSym: ClassSymbol, tpe: Type, tree: Tree)(using Context): Tree =
      tree
        .select(clsSym.requiredMethodRef("onCreate"))
        .withType(TermRef(tpe, clsSym.requiredMethod("onCreate")))
    @tailrec def unapply(tree: Tree)(using Context): Option[ClassSymbol] =
      tree match
        case Select(clsTree @ New(id), _) if clsTree.tpe <:< onCreateEventsTpe =>
          // An object's onCreate is handled under `transformStats`
          if (!id.symbol.is(Module))
            Some(clsTree.tpe.classSymbol.asClass)
          else None
        case Apply(tree, _)     => unapply(tree)
        case TypeApply(tree, _) => unapply(tree)
        case _                  => None
  end OnCreateEventsInstance

  override def prepareForTemplate(tree: Template)(using Context): Context =
    ignore ++= tree.parents
    ctx

  override def transformStats(trees: List[Tree])(using Context): List[Tree] =
    trees.foldRight(List.empty[Tree]) {
      case (tree: ValDef, next :: stats) =>
        val tpe = tree.tpe
        if (tpe <:< onCreateEventsTpe && tpe.typeSymbol.is(Module))
          tree :: next :: OnCreateEventsInstance(tree) :: stats
        else tree :: next :: stats
      case (tree, stats) =>
        tree :: stats
    }
  end transformStats

//  override def transformValDef(tree: tpd.ValDef)(using Context): tpd.Tree =
//    if (tree.name.toString == "xoronx")
//      println(tree.rhs.show)
//      println(tree.rhs)
//
//      val args = List(mkTuple(List(Literal(Constant("x")), Literal(Constant("xvalue")))))
//      val listMapTree =
//        ref(requiredMethod("scala.collection.immutable.ListMap.apply"))
//          .appliedToTypes(List(defn.StringType, defn.AnyType))
//          .appliedToVarargs(args, TypeTree(args.head.tpe))
//      println(listMapTree.show)
//      println(listMapTree)
////      tpd.cpy.ValDef(tree)(rhs = listMapTree)
//      tree
//    else tree
  override def transformApply(tree: Apply)(using Context): Tree =
    if (tree.tpe.isParameterless && !ignore.exists(i => i.sameTree(tree)))
      tree match
        case OnCreateEventsInstance(clsSym) =>
          OnCreateEventsInstance(clsSym, tree.tpe, tree)
        case _ => tree
    else tree

  override def prepareForUnit(tree: Tree)(using Context): Context =
    super.prepareForUnit(tree)
    onCreateEventsTpe = requiredClassRef("dfhdl.internals.OnCreateEvents")
    hasNamePosTpe = requiredClassRef("dfhdl.internals.HasNamePos")
    ctx
end OnCreateEventsPhase
