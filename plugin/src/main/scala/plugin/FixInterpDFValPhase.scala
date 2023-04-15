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
import Names.{Designator, *}
import Constants.Constant
import Types.*

import scala.language.implicitConversions
import collection.mutable
import annotation.tailrec

/*
  This phase fixes dataflow value string interpolation extractors that infer types as `String & B[xyz]`.
  This phase detects such occurrences and changes the type to `B[xyz]`.
 */
class FixInterpDFValPhase(setting: Setting) extends CommonPhase:
  import tpd._

  val phaseName = "FixInterpDFValPhase"

  override val runsAfter = Set("typer")
  override val runsBefore = Set("inlinedPositions")
//  override val debugFilter: String => Boolean =
//    _.contains("DFMatchSpec.scala")
  var dfValTpe: Type = _

  object StripAndString:
    def unapply(tpe: Type)(using Context): Option[Type] =
      tpe.simple match
        case AndType(str, t) if str =:= defn.StringType && t <:< requiredClassRef("dfhdl.hdl.B") =>
          Some(t)
        case _ => None
  end StripAndString

  val updateNamedType = mutable.Map.empty[Name, NamedType]
  override def transformValDef(tree: ValDef)(using Context): tpd.Tree =
    tree.rhs.tpe match
      case StripAndString(t) =>
        val sym = tree.symbol.asTerm
        val ret =
          tpd.ValDef(sym.copy(using ctx)(info = t).asTerm, tree.rhs.withType(t), inferred = false)
        updateNamedType += tree.name -> ret.namedType
        ret
      case _ => tree

  override def transformTypeTree(tree: TypeTree)(using Context): tpd.Tree =
    tree.tpe match
      case StripAndString(t) => TypeTree(t)
      case _                 => tree

  override def transformIdent(tree: Ident)(using Context): tpd.Tree =
    updateNamedType.get(tree.name).map(Ident).getOrElse(tree)

  override def prepareForUnit(tree: Tree)(using Context): Context =
    super.prepareForUnit(tree)
    updateNamedType.clear()
    dfValTpe = requiredClassRef("dfhdl.core.DFVal")
    ctx
end FixInterpDFValPhase
