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
  *   - change infix operator precedence of terms: `a <> b op c` to be `a <> (b op c)` and `a op b
  *     <> c` to be `(a op b) <> c`, where op is `|`, `||`, `&`, `&&`, `^`, or a comparison operator
  *   - change infix operator precedence of terms: `a := b match {...}` to be `a := (b match {...})`
  *     and `a <> b match {...}` to be `a <> (b match {...})`
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

  val opSet = Set("|", "||", "&", "&&", "^", "<<", ">>", "==", "!=", "<", ">", "<=", ">=")
  private val `fix<>andOpPrecedence` = new UntypedTreeMap:
    object InfixOpArgsChange:
      def unapply(tree: InfixOp)(using Context): Option[(Tree, Ident, Tree)] =
        tree match
          case InfixOp(InfixOpArgsChange(a, Ident(conn), b), Ident(op), c)
              if opSet.contains(op.toString) =>
            Some(a, Ident(conn), InfixOp(b, Ident(op), c))
          case InfixOp(a, Ident(op), InfixOpArgsChange(b, Ident(conn), c))
              if opSet.contains(op.toString) =>
            Some(InfixOp(a, Ident(op), b), Ident(conn), c)
          case InfixOp(a, Ident(op), InfixOp(b, Ident(conn), c))
              if conn.toString == "<>" && opSet.contains(op.toString) =>
            Some(InfixOp(a, Ident(op), b), Ident(conn), c)
          case InfixOp(InfixOp(a, Ident(conn), b), Ident(op), c)
              if conn.toString == "<>" && opSet.contains(op.toString) =>
            Some(a, Ident(conn), InfixOp(b, Ident(op), c))
          case _ =>
            None
    end InfixOpArgsChange
    object InfixOpChange:
      def unapply(tree: InfixOp)(using Context): Option[InfixOp] =
        tree match
          case InfixOpArgsChange(a, Ident(conn), b) => Some(InfixOp(a, Ident(conn), Parens(b)))
          case _ =>
            None
    end InfixOpChange
    object MatchAssignOpChange:
      def unapply(tree: Match)(using Context): Option[InfixOp] =
        tree match
          case Match(InfixOp(a, Ident(op), b), cases)
              if op.toString == ":=" || op.toString == "<>" =>
            Some(InfixOp(a, Ident(op), Parens(Match(b, cases))))
          case _ =>
            None
    override def transformBlock(blk: Block)(using Context): Block =
      super.transformBlock(blk) match
        // a connection/assignment could be in return expression position of a Unit-typed block
        case Block(stats, InfixOpChange(expr))       => Block(stats, expr)
        case Block(stats, MatchAssignOpChange(expr)) => Block(stats, expr)
        case blk                                     => blk
    override def transformStats(trees: List[Tree], exprOwner: Symbol)(using Context): List[Tree] =
      super.transformStats(trees, exprOwner).map:
        // only handling pure statements that begin as an infix
        case InfixOpChange(tree)       => tree
        case MatchAssignOpChange(tree) => tree
        case tree                      => tree
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
    object FullSelectGivenName:
      def unapply(tree: Select)(using Context): Option[String] =
        tree match
          case Select(Ident(options), name) if options.toString == "options" =>
            Some(s"options_${name}")
          case Select(FullSelectGivenName(prev), name) => Some(s"${prev}_$name")
          case _                                       => None
    override def transform(tree: Tree)(using Context): Tree =
      super.transform(tree) match
        case tree @ ValDef(_, InfixOpChange(tpt), _) =>
          cpy.ValDef(tree)(tpt = tpt)
        case tree @ DefDef(_, _, InfixOpChange(tpt), _) =>
          cpy.DefDef(tree)(tpt = tpt)
        // workaround https://github.com/scala/scala3/issues/21406
        case tree @ ValDef(name, select: Select, _) if name.isEmpty && tree.mods.is(Given) =>
          select match
            case FullSelectGivenName(updateName) => cpy.ValDef(tree)(name = updateName.toTermName)
            case _                               => tree
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
        case t =>
          t
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
