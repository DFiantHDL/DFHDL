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
  @tailrec private def hasIOVal(tree: Tree)(using Context): Boolean =
    tree match
      case v @ ValDef(_, _, tree) => hasIOVal(v.rhs)
      case InfixOp(_, Ident(connOp), Ident(modifier)) =>
        val modStr = modifier.toString
        connOp.toString == "<>" && (modStr == "IN" | modStr == "OUT" | modStr == "VAR")
      case InfixOp(tree, _, _) => hasIOVal(tree)
      case _                   => false
  private def hasDFC(paramss: List[List[Tree]])(using Context): Boolean =
    paramss.view.flatten.exists {
      case v @ ValDef(_, Ident(n), _) if n.toString == "DFC" & v.mods.flags.is(Given) => true
      case _                                                                          => false
    }

  private val annotMap = new TreeMap(untpd.cpy):
    override def transform(tree: Tree)(using Context): Tree =
      super.transform(tree) match
        case t @ TypeDef(
              _,
              template @ Template(constr @ DefDef(_, paramss, _, _), parents, _, _)
            ) =>
          val isDFContainer = parents.headOption.exists {
            case Ident(n) =>
              n.toString match
                // these classes always require DFC
                case "DFDesign" | "RTDesign" | "DFInterface" | "RTInterface" => true
                case _                                                       => false
            case _ => false
          }
          lazy val skipTestContainer = parents.headOption.exists {
            case Ident(n) =>
              n.toString match
                // these classes always require DFC
                case "DFSpec" => true
                case _        => false
            case _ => false
          }
          lazy val hasIOVals = template.body.exists { x => hasIOVal(x) }
          val addMissingDFC =
            (isDFContainer || (!skipTestContainer && hasIOVals)) && !hasDFC(paramss)
          val dsnAnnot = t.mods.annotations.collectFirst {
            case Apply(Select(New(Ident(n)), _), _) if (n.toString == "dsn") => t
          }
          if (addMissingDFC)
            val dfcArgBlock = List(
              ValDef("x$1".toTermName, Ident("DFC".toTypeName), EmptyTree)
                .withFlags(Private | Synthetic | ParamAccessor | Given)
            )
            val updatedConstr = cpy.DefDef(constr)(paramss = paramss :+ dfcArgBlock)
            val updatedTemplate =
              cpy.Template(template)(constr = updatedConstr)
            cpy.TypeDef(t)(rhs = updatedTemplate)
          else t
        case tree @ ModuleDef(name, impl) =>
          inContext(localCtx(tree)) {
            cpy
              .asInstanceOf[UntypedTreeCopier]
              .ModuleDef(tree)(name, transform(impl).asInstanceOf[Template])
          }
        case t => t
    override def transformMoreCases(tree: Tree)(using Context): Tree =
      tree
  override def runOn(units: List[CompilationUnit])(using Context): List[CompilationUnit] =
    val parsed = super.runOn(units)
    parsed.foreach { cu => // .filter(_.source.file.path.contains("Example.scala"))
      cu.untpdTree = annotMap.transform(cu.untpdTree)
    }
    parsed
end MacroAnnotation
