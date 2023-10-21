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

class MetaContextPlacer(setting: Setting) extends PluginPhase:
  import untpd.*

  val phaseName = "MetaContextPlacer"

  override val runsAfter = Set("parser")
  override val runsBefore = Set("typer")
  private var debugFlag = false
  private val modifiers = Set("IN", "OUT", "INOUT", "VAR", "VAL")
  private val dfcContainers = Set(
    "DFDesign", "RTDesign", "EDDesign", "DFInterface", "RTInterface", "EDInterface"
  )
  @tailrec private def hasDFVal(tree: Tree)(using Context): Boolean =
    tree match
      case InfixOp(_, Ident(connOp), Ident(modifier)) =>
        connOp.toString == "<>" && modifiers.contains(modifier.toString)
      case InfixOp(tree, _, _) => hasDFVal(tree)
      case _                   => false
  private def hasDFVal(paramss: List[List[Tree]])(using Context): Boolean =
    paramss.view.flatten.exists {
      case ValDef(_, tpt, _) if hasDFVal(tpt) => true
      case _                                  => false
    }
  private def hasDFC(paramss: List[List[Tree]])(using Context): Boolean =
    paramss.view.flatten.exists {
      case v @ ValDef(_, Ident(n), _) if n.toString == "DFC" && v.mods.flags.is(Given) => true
      case _                                                                           => false
    }

  extension (tree: DefDef)
    def addDFCArg(using Context): DefDef =
      val flags =
        if (tree.name.toString == "<init>") Private | Synthetic | ParamAccessor | Given
        else Synthetic | ParamAccessor | Given
      val dfcArgBlock = List(
        ValDef("x$1".toTermName, Ident("DFC".toTypeName), EmptyTree).withFlags(flags)
      )
      untpd.cpy.DefDef(tree)(paramss = tree.paramss :+ dfcArgBlock)

  private val addDFCTreeMap = new UntypedTreeMap:
    protected var extensionWithDFVal: Boolean = false
    protected var extensionWithDFC: Boolean = false
    override def transform(tree: Tree)(using Context): Tree =
      extension (tree: Tree)
        @tailrec def inherits(set: Set[String]): Boolean =
          tree match
            case Ident(n)                 => set.contains(n.toString)
            case Apply(tree, _)           => tree.inherits(set)
            case Select(tree, _)          => tree.inherits(set)
            case New(tree)                => tree.inherits(set)
            case AppliedTypeTree(tree, _) => tree.inherits(set)
      super.transform(tree) match
        case t @ TypeDef(
              _,
              template @ Template(
                constr @ DefDef(_, paramss, _, _),
                parents: List[Tree] @unchecked,
                _,
                _
              )
            ) =>
          val isDFContainer = parents.headOption.exists(_.inherits(dfcContainers))
          lazy val skipTestContainer = parents.headOption.exists(_.inherits(Set("DFSpec")))
          lazy val hasDFVals = template.body.exists {
            case v: ValDef => hasDFVal(v.rhs)
            case _         => false
          }
          val addMissingDFC =
            (isDFContainer || (!skipTestContainer && hasDFVals)) && !hasDFC(paramss)
          if (addMissingDFC)
            val updatedTemplate =
              cpy.Template(template)(constr = constr.addDFCArg)
            cpy.TypeDef(t)(rhs = updatedTemplate)
          else t
        case tree @ ModuleDef(name, impl) =>
          inContext(localCtx(tree)) {
            untpd.cpy.ModuleDef(tree)(name, transform(impl).asInstanceOf[Template])
          }
        case tree @ DefDef(name, paramss, tpt, _)
            if (extensionWithDFVal || (name.toString != "<init>" && (hasDFVal(tpt) || hasDFVal(
              paramss
            )))) &&
              !(extensionWithDFC || hasDFC(paramss)) =>
          tree.addDFCArg
        case t => t
      end match
    end transform
    override def transformMoreCases(tree: Tree)(using Context): Tree =
      tree match
        case ExtMethods(paramss, _) =>
          val backup = (extensionWithDFVal, extensionWithDFC)
          extensionWithDFVal = hasDFVal(paramss)
          extensionWithDFC = hasDFC(paramss)
          val ret = super.transformMoreCases(tree)
          extensionWithDFVal = backup._1
          extensionWithDFC = backup._2
          ret
        case _ => super.transformMoreCases(tree)

  object IntConnVAL:
    def unapply(tree: InfixOp)(using Context): Option[InfixOp] =
      tree match
        case InfixOp(Ident(intName), c @ Ident(connName), v @ Ident(valName))
            if intName.toString == "Int" && connName.toString == "<>" && valName.toString == "VAL" =>
          val intReplacement = AppliedTypeTree(
            Ident("SInt".toTypeName),
            List(SingletonTypeTree(Literal(Constant(32))))
          )
          Some(InfixOp(intReplacement, c, v))
        case _ => None
  private val fixIntValModifierTreeMap = new UntypedTreeMap:
    override def transform(tree: Tree)(using Context): Tree =
      super.transform(tree) match
        case tree @ ValDef(_, IntConnVAL(fixedInfix), _) =>
          cpy.ValDef(tree)(tpt = fixedInfix)
        case tree @ DefDef(_, _, IntConnVAL(fixedInfix), _) =>
          cpy.DefDef(tree)(tpt = fixedInfix)
        case t => t
      end match
    end transform
  override def runOn(units: List[CompilationUnit])(using Context): List[CompilationUnit] =
    val parsed = super.runOn(units)
    parsed.foreach { cu =>
      // debugFlag = cu.source.file.path.contains("Example.scala")
      cu.untpdTree = addDFCTreeMap.transform(cu.untpdTree)
      cu.untpdTree = fixIntValModifierTreeMap.transform(cu.untpdTree)
    }
    parsed
end MetaContextPlacer
