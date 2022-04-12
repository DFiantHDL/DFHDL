package DFiant.compiler.stages
import DFiant.compiler.ir.*
import DFiant.internals.*

abstract class Stage(db: DB):
  protected def preTransform: DB = db
  final val designDB: DB = preTransform
  export designDB.getSet
  def transform: DB

abstract class Stage2 extends Product, Serializable, HasTypeName:
  private var getSet: MemberGetSet = _
  final protected given MemberGetSet = getSet
  final lazy val depSet: Set[Stage2] = dependencies.toSet
  final def run(designDB: DB): DB =
    getSet = designDB.getSet
    transform(designDB)
  def dependencies: List[Stage2]
  def nullifies: Set[Stage2]
  protected def transform(designDB: DB): DB

trait HasDB[T]:
  def apply(t: T): DB
object HasDB:
  given HasDB[DB] with
    def apply(t: DB): DB = t
  given [T <: DFiant.core.Design]: HasDB[T] with
    def apply(t: T): DB = t.getDB

extension [T: HasDB](t: T) def db: DB = summon[HasDB[T]](t)
