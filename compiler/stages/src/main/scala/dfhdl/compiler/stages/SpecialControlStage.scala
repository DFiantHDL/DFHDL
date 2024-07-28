package dfhdl.compiler.stages

import dfhdl.compiler.analysis.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.patching.*
import dfhdl.options.CompilerOptions

sealed trait SpecialControlStage extends Stage

//special cased within StageRunner to disable sanity checks after theses changes under trace logging.
trait NoCheckStage extends SpecialControlStage

abstract class BundleStage(deps: Stage*) extends NoCheckStage:
  override def dependencies: List[Stage] = deps.toList
  override def nullifies: Set[Stage] = Set()
  def transform(designDB: DB)(using MemberGetSet, CompilerOptions): DB = designDB
