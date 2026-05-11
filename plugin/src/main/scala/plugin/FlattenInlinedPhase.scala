package dfhdl.plugin

import dotty.tools.dotc.*
import plugins.*
import core.*
import Contexts.*
import Symbols.*
import Flags.*
import ast.tpd
import ast.tpd.*

// This phase optimizes trees produced by transparent inline expansion to reduce
// compile time in the posttyper phase. It performs two key optimizations:
//
// 1. Flattens deeply nested Inlined trees from chained transparent inline
//    operations (e.g., a + b + c + d).
//
// 2. Replaces call trees in Inlined nodes with minimal call traces (just a
//    reference to the top-level class). This prevents posttyper from
//    re-transforming complex call trees (which may contain TypeApply with
//    heavy type arguments) for each of the many Inlined nodes produced
//    by chained inline expansions.
class FlattenInlinedPhase(setting: Setting) extends PluginPhase:
  import tpd.*

  val phaseName = "FlattenInlined"
  override val runsAfter = Set("typer")
  override val runsBefore = Set("posttyper")

  // Pre-compute the call trace that posttyper would produce.
  // This replaces a complex call tree (e.g., TypeApply(Select(...), ...))
  // with a minimal Ident/Select pointing to the top-level class.
  private def minimizeCall(call: Tree)(using Context): Tree =
    if call.isEmpty then call
    else
      val callSym = call.symbol
      if !callSym.exists then call
      else
        val topLevelCls = callSym.topLevelClass
        if !topLevelCls.exists then call
        else if callSym.is(Macro) then
          ref(topLevelCls.owner).select(topLevelCls.name)(using
            ctx.withOwner(topLevelCls.owner)
          ).withSpan(call.span)
        else
          Ident(topLevelCls.typeRef).withSpan(call.span)

  private val treeMap = new TreeMap:
    override def transform(tree: Tree)(using Context): Tree =
      super.transform(tree) match
        case inlined @ Inlined(call, bindings, expansion) =>
          val (innerBindings, innerExpansion) = flattenInlined(expansion)
          val newCall = minimizeCall(call)
          val allBindings = bindings ++ innerBindings
          if (newCall eq call) && innerBindings.isEmpty then inlined
          else cpy.Inlined(inlined)(newCall, allBindings, innerExpansion)
        case tree => tree

  private def flattenInlined(tree: Tree)(using Context): (List[MemberDef], Tree) =
    tree match
      case Inlined(_, bindings, expansion) =>
        val (innerBindings, innerExpansion) = flattenInlined(expansion)
        (bindings ++ innerBindings, innerExpansion)
      case Block(stats, expr) =>
        val (innerBindings, innerExpr) = flattenInlined(expr)
        if innerBindings.isEmpty then (Nil, tree)
        else
          val allStats = stats.map(_.asInstanceOf[MemberDef]) ++ innerBindings
          (Nil, cpy.Block(tree)(allStats.toList, innerExpr))
      case _ => (Nil, tree)

  override def transformUnit(tree: Tree)(using Context): Tree =
    treeMap.transform(tree)
end FlattenInlinedPhase
