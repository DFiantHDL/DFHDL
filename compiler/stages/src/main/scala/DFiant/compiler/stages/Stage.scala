package DFiant.compiler.stages
import DFiant.compiler.ir.*
import DFiant.internals.*

abstract class Stage(db: DB):
  protected def preTransform: DB = db
  final val designDB: DB = preTransform
  export designDB.getSet
  def transform: DB

abstract class Stage2 extends Product, Serializable, HasTypeName:
  def dependencies: List[Stage2]
  lazy val depSet: Set[Stage2] = dependencies.toSet
  def nullifies: Set[Stage2]
  def apply(designDB: DB)(using MemberGetSet): DB

trait HasDB[T]:
  def apply(t: T): DB
object HasDB:
  given HasDB[DB] with
    def apply(t: DB): DB = t
  given [T <: DFiant.core.Design]: HasDB[T] with
    def apply(t: T): DB = t.getDB

extension [T: HasDB](t: T) def db: DB = summon[HasDB[T]](t)
