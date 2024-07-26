package dfhdl.compiler.stages

import dfhdl.compiler.analysis.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.patching.*
import dfhdl.options.CompilerOptions

sealed abstract class SpecialControlStage extends Stage

abstract class BundleStage(deps: Stage*) extends SpecialControlStage:
  override def dependencies: List[Stage] = deps.toList
  override def nullifies: Set[Stage] = Set()
  def transform(designDB: DB)(using MemberGetSet, CompilerOptions): DB = designDB

//special cased within StageRunner to disable sanity checks after theses changes under trace logging.
trait NoCheckStage extends Stage
