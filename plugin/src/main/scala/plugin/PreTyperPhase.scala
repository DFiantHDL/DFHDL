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

/** This is a pre-typer phase that does very minor things:
  *   - change infix operator precedence of type signature: `a X b <> c` to be `(a X b) <> c`
  *   - change infix operator precedence of terms: `a <> b op c` to be `a <> (b op c)`, where op is
  *     `|`, `||`, `&`, `&&`, or `^`
  *   - workaround for https://github.com/scala/scala3/issues/20053
  */
class PreTyperPhase(setting: Setting) extends PluginPhase:
  import untpd.*

  val phaseName = "PreTyper"

  override val runsAfter = Set("parser")
  override val runsBefore = Set("typer")
  private var debugFlag = false

  def debug(str: => Any*): Unit =
    if (debugFlag) println(str.mkString(", "))

  val opSet = Set("|", "||", "&", "&&", "^")
  private val `fix<>andOpPrecedence` = new UntypedTreeMap:
    object InfixOpArgsChange:
      def unapply(tree: InfixOp)(using Context): Option[(Tree, Ident, Tree)] =
        tree match
          case InfixOp(InfixOpArgsChange(a, Ident(conn), b), Ident(op), c)
              if opSet.contains(op.toString) =>
            Some(a, Ident(conn), InfixOp(b, Ident(op), c))
          case InfixOp(InfixOp(a, Ident(conn), b), Ident(op), c)
              if conn.toString == "<>" && opSet.contains(op.toString) =>
            Some(a, Ident(conn), InfixOp(b, Ident(op), c))
          case _ => None
    object InfixOpChange:
      def unapply(tree: InfixOp)(using Context): Option[InfixOp] =
        tree match
          case InfixOpArgsChange(a, Ident(conn), b) => Some(InfixOp(a, Ident(conn), Parens(b)))
          case _                                    => None
    end InfixOpChange
    override def transformBlock(blk: Block)(using Context): Block =
      super.transformBlock(blk) match
        // a connection could be in return expression position of a Unit-typed block
        case Block(stats, InfixOpChange(expr)) => Block(stats, expr)
        case blk                               => blk
    override def transformStats(trees: List[Tree], exprOwner: Symbol)(using Context): List[Tree] =
      super.transformStats(trees, exprOwner).map:
        // only handling pure statements that begin as an infix
        case InfixOpChange(tree) => tree
        case tree =>
          debug(tree)
          tree
    override def transform(tree: Tree)(using Context): Tree =
      super.transform(tree) match
        // a connection could be in return position of a DFHDL Unit definition (if no block is used)
        case tree @ DefDef(_, _, _, InfixOpChange(rhs)) =>
          cpy.DefDef(tree)(rhs = rhs)
        case t => t
      end match
    end transform

  val reduceOpSet = Set("|", "&", "^")
  val warnOpSet = Set("reduce|", "reduce&", "reduce^")
  private val `fixXand<>Precedence` = new UntypedTreeMap:
    object InfixOpChange:
      def unapply(tree: InfixOp)(using Context): Option[InfixOp] =
        tree match
          case InfixOp(a, Ident(x), InfixOp(b, Ident(conn), c))
              if x.toString == "X" && conn.toString == "<>" =>
            Some(InfixOp(Parens(InfixOp(a, Ident(x), b)), Ident(conn), c))
          case _ => None
    override def transform(tree: Tree)(using Context): Tree =
      super.transform(tree) match
        case tree @ ValDef(_, InfixOpChange(tpt), _) =>
          cpy.ValDef(tree)(tpt = tpt)
        case tree @ DefDef(_, _, InfixOpChange(tpt), _) =>
          cpy.DefDef(tree)(tpt = tpt)
        // workaround https://github.com/scala/scala3/issues/20053
        case tree @ Select(t, name) =>
          val nameStr = name.toString()
          if (reduceOpSet.contains(nameStr)) cpy.Select(tree)(t, s"reduce$nameStr".toTermName)
          else if (warnOpSet.contains(nameStr))
            report.error(
              s"do not directly call `.$nameStr`; instead call `.${nameStr.last}`",
              tree.srcPos
            )
            tree
          else tree
        case t => t
      end match
    end transform

  override def runOn(units: List[CompilationUnit])(using Context): List[CompilationUnit] =
    val parsed = super.runOn(units)
    parsed.foreach { cu =>
      // debugFlag = cu.source.file.path.contains("Example.scala")
      cu.untpdTree = `fix<>andOpPrecedence`.transform(cu.untpdTree)
      cu.untpdTree = `fixXand<>Precedence`.transform(cu.untpdTree)
    }
    parsed
end PreTyperPhase
