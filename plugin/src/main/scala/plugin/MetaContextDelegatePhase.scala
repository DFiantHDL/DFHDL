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
import ast.tpd
import StdNames.nme
import Names._
import Constants.Constant
import Types._
import scala.language.implicitConversions
import collection.mutable
import annotation.tailrec

class MetaContextDelegatePhase(setting: Setting) extends CommonPhase:
  import tpd._

  val phaseName = "MetaContextDelegate"

  override val runsAfter = Set("MetaContextGen")
  override val runsBefore = Set(transform.FirstTransform.name)
  val ignore = mutable.Set.empty[String]

  override def transformApply(tree: Apply)(using Context): Tree =
    if (tree.tpe.isParameterless && tree.isContextDelegate) tree match
      case ApplyFunArgs(fun, ((lhs: Apply) :: Nil) :: rhsArgs :: Nil) =>
        lhs match
          case ContextArg(argTree) =>
            val clsSym = argTree.tpe.typeSymbol
            val methodSym = clsSym.requiredMethod(fun.symbol.name.toString)
            val delegated = argTree.select(methodSym).appliedToArgs(rhsArgs)
            lhs.replaceArg(argTree, delegated)
          case _ =>
            tree
      case _ => tree
    else tree

  override def prepareForUnit(tree: Tree)(using Context): Context =
    super.prepareForUnit(tree)
    ctx
end MetaContextDelegatePhase
