package dfhdl.compiler.stages
import dfhdl.compiler.ir.*
import dfhdl.internals.*
import dfhdl.options.CompilerOptions
import dfhdl.core.{Design}

trait Stage extends Product, Serializable, HasTypeName derives CanEqual:
  final lazy val depSet: Set[Stage] = dependencies.toSet
  def dependencies: List[Stage]
  def nullifies: Set[Stage]
  def runCondition(using CompilerOptions): Boolean = true
  def transform(designDB: DB)(using MemberGetSet, CompilerOptions): DB

trait HasDB[T]:
  def apply(t: T): DB
object HasDB:
  given HasDB[DB] with
    def apply(t: DB): DB = t
  given [D <: Design]: HasDB[D] with
    def apply(t: D): DB = t.getDB
  given HasDB[StagedDesign] with
    def apply(t: StagedDesign): DB = t.stagedDB
  given HasDB[CompiledDesign] with
    def apply(t: CompiledDesign): DB = t.staged.stagedDB

extension [T: HasDB](t: T) def db: DB = summon[HasDB[T]](t)
