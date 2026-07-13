package dfhdl.compiler.ir

import upickle.default.*
import scala.collection.mutable

/** The cache key of a sub-design entry: the def's owner class (the entry's anchor in the cache
  * service, which completes the cross-run code identity from it) and the elaboration-computed
  * content key of the instantiation (`DesignLoadKey.localKey`).
  */
final case class SubDesignRef(ownerClassName: String, localKey: String) derives ReadWriter

/** The cross-run cache artifact of ONE design: the design's own sub-DB and, per instantiated child
  * design, the cache key of that child's own entry.
  *
  * Children are referenced, NOT embedded: the loading run resolves each child through the design
  * load gate exactly like a live instantiation, so a design instantiated by several parents is
  * loaded ONCE and unifies with a live elaboration of the same key. Embedding child bodies instead
  * would duplicate every shared descendant (one copy per adopting parent).
  */
final case class SubDesignEntry(
    db: DB,
    children: List[(StaticRef, SubDesignRef)]
) derives ReadWriter:
  /** This entry's design as a design of the loading run. Only the design's STRUCTURAL KEYS are
    * re-anchored, since only they cross a sub-DB boundary: the design block takes `topToken` (its
    * `subDBs` key, and the target of the instantiating `DFDesignInst.designRef`), and each
    * instantiated child's `designRef` is retargeted to the design block the loading run resolved
    * for it (`childTokens`, keyed by the child's `designRef` AS STORED).
    *
    * The entry's own ref TOKENS are kept exactly as stored, even though the storing run minted them
    * from its own `RefGen` and they therefore collide, by value, with this run's tokens. They can:
    * a sub-DB's refTable is self-contained, so a token only ever resolves within the sub-DB that
    * emits it. The one place that merges refTables across sub-DBs is the legacy flat view
    * (`DB.newToOld`), and it re-mints colliding tokens itself (see `DB.freshenLocalRefs`).
    *
    * Global members likewise keep their stored identity, so the same global reached through two
    * different entries stays ONE member (globals unify by value across sub-DBs).
    */
  def cloneForAdoption(
      topToken: DFOwner.Ref,
      childTokens: Map[StaticRef, DFOwner.Ref]
  ): DB =
    val oldTop = db.top
    val memberMap = mutable.Map.empty[DFMember, DFMember]
    val newMembers = db.members.map {
      case d: DFDesignBlock if d eq oldTop =>
        val newTop = d.copy(ownerRef = topToken)
        memberMap(d) = newTop
        newTop
      case inst: DFDesignInst =>
        val newInst = inst.copy(designRef = StaticRef(childTokens(inst.designRef)))
        memberMap(inst) = newInst
        newInst
      case m => m
    }
    // the stored table, with the top's own owner token re-keyed and every rewritten member
    // re-pointed (a member is a VALUE: the block and the instances changed)
    val oldTopOwnerRef: DFRefAny = oldTop.ownerRef
    val newRefTable = db.refTable.map { (ref, target) =>
      val key: DFRefAny = if (ref == oldTopOwnerRef) topToken else ref
      key -> memberMap.getOrElse(target, target)
    }
    DB(newMembers, newRefTable, db.globalTags, Nil)
  end cloneForAdoption
end SubDesignEntry

object SubDesignEntry:
  extension (entry: SubDesignEntry) def toJsonString: String = write(entry)
  def fromJsonString(json: String): SubDesignEntry = read[SubDesignEntry](json)
