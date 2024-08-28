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
  // override val debugFilter: String => Boolean = _.contains("Example.scala")
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
            case Some(topAnnotTree @ Apply(Apply(_, topAnnotOptionsTrees), _)) =>
              // genMain argument in top annotation is true by default
              val genMain = topAnnotOptionsTrees match
                case Literal(Constant(genMain: Boolean)) :: Nil => genMain
                case _                                          => true
              if (genMain)
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
                if (template.constr.paramss.length > 1)
                  report.error(
                    "Unsupported multiple parameter blocks for top-level design.",
                    template.constr.srcPos
                  )
                val designNameTree = Literal(Constant(tn.toString))
                val topScalaPathTree = Literal(Constant(dfApp.fullName.toString()))
                val paramVDs =
                  template.constr.paramss.flatten.collect { case vd: ValDef => vd }
                val dsnArgNames = mkList(paramVDs.map(vd => Literal(Constant(vd.name.toString))))
                extension (vd: Type)
                  def argType: Option[String] =
                    if (vd <:< defn.IntType || vd <:< dfConstInt32Tpe) Some("Int")
                    else if (vd <:< defn.StringType) Some("String")
                    else if (vd <:< defn.DoubleType) Some("Double")
                    else if (vd <:< defn.BooleanType || vd <:< dfConstBoolTpe) Some("Boolean")
                    else if (vd <:< dfConstBitTpe) Some("Bit")
                    else None

                val dsnArgTypes = mkList(paramVDs.map { vd =>
                  vd.tpt.tpe.argType match
                    case Some(value) => Literal(Constant(value))
                    case _ =>
                      report.error(
                        "Unsupported argument's type for top-level design with a default app entry point.\nEither use a supported type or disable the app entry point generation with `@top(false)`.",
                        vd.srcPos
                      )
                      EmptyTree
                })
                val defaultMap = mutable.Map.empty[Int, Tree]
                rest match
                  case (_: ValDef) :: (compSym @ TypeDef(_, compTemplate: Template)) :: _
                      if compSym.symbol.companionClass == clsSym =>
                    compTemplate.body.foreach {
                      case dd @ DefDef(NameKinds.DefaultGetterName(n, i), _, _, _) =>
                        defaultMap += i -> ref(dd.symbol)
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
                    designNameTree, topScalaPathTree, topAnnotTree, dsnArgNames, dsnArgTypes,
                    dsnArgValues, dsnArgDescs
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
