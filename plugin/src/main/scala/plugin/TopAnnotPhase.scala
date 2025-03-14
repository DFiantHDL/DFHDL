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
import dotty.tools.dotc.semanticdb.BooleanConstant

/*
  This phase creates a `top_<design_name>` object with a `DFApp` main entry point
  for each design that has a `@top` annotation. The design parameters must have
  default arguments and have the supported types. These arguments can then be
  overridden by the command-line options that `DFApp` application provides.
 */
class TopAnnotPhase(setting: Setting) extends CommonPhase:
  import tpd._

  val phaseName = "TopAnnot"

  override val runsAfter = Set("typer")
  override val runsBefore = Set("MetaContextGen")
  // override val debugFilter: String => Boolean = _.contains("Playground.scala")
  var topAnnotSym: ClassSymbol = uninitialized
  var appTpe: TypeRef = uninitialized
  var dfConstBoolTpe: TypeRef = uninitialized
  var dfConstBitTpe: TypeRef = uninitialized
  var dfConstInt32Tpe: TypeRef = uninitialized

  override def transformStats(trees: List[Tree])(using Context): List[Tree] =
    val retTrees = mutable.ListBuffer.empty[Tree]
    var explored: List[Tree] = trees
    while (explored.nonEmpty)
      explored match
        case (td @ TypeDef(tn, template: Template)) :: rest if td.tpe <:< hasDFCTpe =>
          val clsSym = td.symbol.asClass
          // has top annotation and no companion object
          clsSym.getAnnotation(topAnnotSym).map(a => dropProxies(a.tree)) match
            case Some(topAnnotTree @ Apply(Apply(Apply(_, topAnnotOptionsTrees), _), _)) =>
              // genMain argument in top annotation is true by default
              val genMain = topAnnotOptionsTrees match
                case Literal(Constant(genMain: Boolean)) :: _ => genMain
                case NamedArg(genMainName, Literal(Constant(genMain: Boolean))) :: _
                    if genMainName.toString == "genMain" =>
                  genMain
                case _ => true
              if (genMain)
                if (template.constr.paramss.length > 1)
                  report.error(
                    "Unsupported multiple parameter blocks for top-level design.",
                    template.constr.srcPos
                  )
                  retTrees += td
                  explored = rest
                else
                  // the top entry point module symbol
                  val dfApp = newCompleteModuleSymbol(
                    ctx.owner,
                    s"top_${tn}".toTermName,
                    Touched,
                    Touched | NoInits,
                    List(defn.ObjectType, appTpe),
                    Scopes.newScope,
                    coord = topAnnotTree.span,
                    compUnitInfo = clsSym.compUnitInfo
                  )
                  val moduleCls = dfApp.moduleClass.asClass
                  val designNameTree = Literal(Constant(tn.toString))
                  val topScalaPathTree = Literal(Constant(dfApp.fullName.toString()))
                  val paramVDs =
                    template.constr.paramss.flatten.collect { case vd: ValDef => vd }
                  val dsnArgNames = mkList(paramVDs.map(vd => Literal(Constant(vd.name.toString))))
                  val defaultMap = mutable.Map.empty[Int, Tree]
                  rest match
                    case (module: ValDef) :: (compSym @ TypeDef(_, compTemplate: Template)) :: _
                        if compSym.symbol.companionClass == clsSym =>
                      compTemplate.body.foreach {
                        case dd @ DefDef(NameKinds.DefaultGetterName(n, i), _, _, _) =>
                          defaultMap += i -> ref(module.symbol).select(dd.symbol)
                        case _ =>
                      }
                    case _ =>
                  val dsnArgValues =
                    mkList(
                      paramVDs.zipWithIndex.map((vd, i) =>
                        defaultMap.get(i) match
                          case Some(value) => value
                          case None =>
                            report.error(
                              "Missing argument's default value for top-level design with a default app entry point.\nEither add a default value or disable the app entry point generation with `@top(false)`.",
                              vd.srcPos
                            )
                            EmptyTree
                      )
                    )
                  val dsnArgDescs =
                    mkList(paramVDs.map(vd => Literal(Constant(vd.symbol.docString.getOrElse("")))))
                  val setInitials = This(moduleCls).select("setInitials".toTermName).appliedToArgs(
                    List(
                      designNameTree, topScalaPathTree, topAnnotTree, dsnArgNames, dsnArgValues,
                      dsnArgDescs
                    )
                  )
                  val dsnInstArgs = paramVDs.map(vd =>
                    This(moduleCls).select("getDsnArg".toTermName).appliedTo(
                      Literal(Constant(vd.name.toString))
                    )
                  )
                  val dsnInst = New(clsSym.typeRef, dsnInstArgs)
                  val setDsn = This(moduleCls).select("setDsn".toTermName).appliedTo(dsnInst)
                  retTrees ++= td :: ModuleDef(dfApp, List(setInitials, setDsn)).trees
                  explored = rest
                end if
              else
                retTrees += td
                explored = rest
              end if
            case _ =>
              retTrees += td
              explored = rest
          end match
        case _ =>
          retTrees += explored.head
          explored = explored.drop(1)
    end while
    retTrees.toList
  end transformStats

  override def prepareForUnit(tree: Tree)(using Context): Context =
    super.prepareForUnit(tree)
    topAnnotSym = requiredClass("dfhdl.top")
    appTpe = requiredClassRef("dfhdl.app.DFApp")
    dfConstBoolTpe = requiredClassRef("dfhdl.core.DFConstBool")
    dfConstBitTpe = requiredClassRef("dfhdl.core.DFConstBit")
    dfConstInt32Tpe = requiredClassRef("dfhdl.core.DFConstInt32")
    ctx
end TopAnnotPhase
