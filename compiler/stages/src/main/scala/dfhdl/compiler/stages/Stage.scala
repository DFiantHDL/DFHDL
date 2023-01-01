package dfhdl.compiler.stages
import dfhdl.compiler.ir.*
import dfhdl.internals.*
import dfhdl.core.{Design, StagedDesign}

abstract class Stage extends Product, Serializable, HasTypeName:
  final lazy val depSet: Set[Stage] = dependencies.toSet
  def dependencies: List[Stage]
  def nullifies: Set[Stage]
  def transform(designDB: DB)(using MemberGetSet): DB

trait HasDB[T]:
  def apply(t: T): DB
object HasDB:
  given HasDB[DB] with
    def apply(t: DB): DB = t
  given [D <: Design]: HasDB[D] with
    def apply(t: D): DB = t.getDB
  given [D <: Design]: HasDB[StagedDesign[D]] with
    def apply(t: StagedDesign[D]): DB = t.stagedDB

extension [T: HasDB](t: T) def db: DB = summon[HasDB[T]](t)
