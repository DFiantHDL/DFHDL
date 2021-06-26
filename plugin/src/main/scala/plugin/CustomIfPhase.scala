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
import ast.tpd
import StdNames.nme
import Names._
import Constants.Constant
import Types._
import scala.language.implicitConversions
import collection.mutable
import annotation.tailrec

class CustomIfPhase(setting: Setting) extends CommonPhase {
  import tpd._

  val phaseName = "CustomIf"

  override val runsAfter = Set("MetaContextGen")
  override val runsBefore = Set(transform.FirstTransform.name)
  val ignore = mutable.Set.empty[String]

  override def transformApply(tree: Apply)(using Context): Tree =
    tree

  override def prepareForIf(tree: If)(using Context): Context =
//    println(tree)
    ctx

  override def prepareForUnit(tree: Tree)(using Context): Context =
    super.prepareForUnit(tree)
    ctx

}
