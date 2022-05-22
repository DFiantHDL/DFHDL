package dfhdl.compiler.stages
import dfhdl.compiler.ir.*
import dfhdl.internals.*

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
  given [T <: dfhdl.core.Design]: HasDB[T] with
    def apply(t: T): DB = t.getDB

extension [T: HasDB](t: T) def db: DB = summon[HasDB[T]](t)
