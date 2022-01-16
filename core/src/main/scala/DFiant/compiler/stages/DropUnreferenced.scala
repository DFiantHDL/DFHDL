package DFiant.compiler.stages
import DFiant.compiler.ir.*
import DFiant.compiler.patching.*
import DFiant.compiler.analysis.*

private class DropUnreferenced(db: DB) extends Stage(db):
  override def transform: DB =
    val patchList = designDB.members.collect {
      case m @ NewVar() if !designDB.memberTable.contains(m) && m.externalInit.isEmpty =>
        m -> Patch.Remove
    }
    designDB.patch(patchList)

extension [T: HasDB](t: T)
  def dropUnreferenced: DB = new DropUnreferenced(summon[HasDB[T]](t)).transform
