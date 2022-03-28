package DFiant.plugin

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

class MacroAnnotation(setting: Setting) extends PluginPhase:
  import untpd.*

  val phaseName = "MacroAnnotation"

  override val runsAfter = Set("parser")
  override val runsBefore = Set("typer")

  private val annotMap = new TreeMap(untpd.cpy):
    override def transform(tree: Tree)(using Context): Tree =
      tree match
        case t @ TypeDef(
              _,
              template @ Template(constr @ DefDef(_, paramss, _, _), parents, _, _)
            ) =>
          val dsnAnnot = t.mods.annotations.collectFirst {
            case Apply(Select(New(Ident(n)), _), _) if (n.toString == "dsn") => t
          }
          dsnAnnot
            .map(a =>
              val dfcArgBlock = List(
                ValDef("x$1".toTermName, Ident("DFC".toTypeName), EmptyTree)
                  .withFlags(Private | Synthetic | ParamAccessor | Given)
              )
              val updatedConstr = cpy.DefDef(constr)(paramss = paramss :+ dfcArgBlock)
              val updatedParents =
                if (parents.isEmpty) List(Ident("DFDesign".toTypeName)) else parents
              val updatedTemplate =
                cpy.Template(template)(constr = updatedConstr, parents = updatedParents)
              cpy.TypeDef(t)(rhs = updatedTemplate)
            )
            .getOrElse(tree)
        case _ =>
          super.transform(tree)
    override def transformMoreCases(tree: Tree)(using Context): Tree =
      tree
  override def runOn(units: List[CompilationUnit])(using Context): List[CompilationUnit] =
    val parsed = super.runOn(units)
    parsed.foreach { cu =>
      cu.untpdTree = annotMap.transform(cu.untpdTree)
    }
    parsed
end MacroAnnotation
