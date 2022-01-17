package DFiant.compiler.stages
import DFiant.compiler.ir.DB

abstract class Stage(db: DB):
  protected def preTransform: DB = db
  final val designDB: DB = preTransform
  export designDB.getSet
  def transform: DB

trait HasDB[T]:
  def apply(t: T): DB
object HasDB:
  given HasDB[DB] with
    def apply(t: DB): DB = t
  given [T <: DFiant.core.DFDesign]: HasDB[T] with
    def apply(t: T): DB = t.getDB

extension [T: HasDB](t: T) def db: DB = summon[HasDB[T]](t)
