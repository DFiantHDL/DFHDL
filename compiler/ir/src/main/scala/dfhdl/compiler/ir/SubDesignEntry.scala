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

  /** This entry's design as a design of the LOADING run.
    *
    * A stored ref token means nothing to this run. The storing run minted it from its own
    * generator, whose ids restart per run, so an entry arrives holding tokens this run will mint
    * again for its own members: not a remote coincidence but the norm, since both runs elaborate
    * the same code. So every token the entry holds is re-minted here, from the loading run's
    * generator, and the design is threaded onto this run's hierarchy: the design block takes
    * `topToken` (its `subDBs` key, and the target of the instantiating `DFDesignInst.designRef`)
    * and each instantiated child's `designRef` is retargeted to the design this run resolved for it
    * (`childTokens`, keyed by the child's `designRef` AS STORED).
    *
    * The re-minting and the re-anchoring are ONE pass, deliberately. Re-anchoring first would key
    * the table on a minted `topToken` while the stored tokens are still in it, and a minted token
    * equal to a stored one silently merges two bindings into one (the design block's owner, which
    * resolves to nothing, over a port's owner, which resolves to the design). Doing them together
    * means no stored token ever shares the table with a minted one.
    *
    * Global members keep their stored identity, so the same global reached through two different
    * entries stays ONE member (globals unify by value across sub-DBs).
    */
  def cloneForAdoption(
      topToken: DFOwner.Ref,
      childTokens: Map[StaticRef, DFOwner.Ref]
  )(using RefGen): DB =
    given MemberGetSet = db.getSet
    val oldTop = db.top
    // BY VALUE: a deserialized DB's refTable targets are distinct objects from the members they
    // name (the members list and the table are rebuilt independently), and only value equality
    // re-unites them with their fresh copies.
    val memberMap = mutable.Map.empty[DFMember, DFMember]
    // rebuilt member by member (every ref the fresh members emit, and only those), NOT by mapping
    // the stored table through an old-ref -> new-ref map: members can SHARE a ref token (they
    // share the DFType object that emits it) and each fresh copy mints its own token for it, so a
    // single map would keep the last one only and leave every other copy's ref unbound
    val newRefTable = mutable.Map.empty[DFRefAny, DFMember]
    db.members.foreach {
      // a global is shared by identity across every sub-DB that reaches it, tokens included: it
      // stays exactly as stored, and so do the refs it emits
      case g: DFVal.CanBeGlobal if g.isGlobal =>
        memberMap(g) = g
        g.getAllRefs.foreach(r => db.refTable.get(r).foreach(t => newRefTable(r) = t))
      case m =>
        val fresh = m match
          // the structural keys are this run's, and they are what the hierarchy is threaded on
          case d: DFDesignBlock if d eq oldTop =>
            d.copyWithNewRefs.copy(ownerRef = topToken)
          case inst: DFDesignInst =>
            inst.copyWithNewRefs.copy(designRef = StaticRef(childTokens(inst.designRef)))
          // a subprogram call is re-anchored exactly like an instance: its key (which the
          // pairwise ref freshening below never touches) is retargeted to the design this
          // run resolved for the called child
          case DFVal.Func.Call(func, storedKey) =>
            func.copyWithNewRefs.copy(op = DFVal.Func.Op.Def(StaticRef(childTokens(storedKey))))
          case m => m.copyWithNewRefs
        memberMap(m) = fresh
        // pairwise (oldRef -> newRef) through the symmetric `getAllRefs` enumeration (which
        // `copyWithNewRefs` freshens in that same order), each fresh ref taking over the stored
        // ref's binding. `DFDesignInst.designRef` is not part of it (it is resolved structurally,
        // through the child's `subDBs` key), which is why retargeting it above disturbs nothing.
        m.getAllRefs.lazyZip(fresh.getAllRefs).foreach { (o, n) =>
          db.refTable.get(o).foreach(target => newRefTable(n) = target)
        }
    }
    DB(
      db.members.map(memberMap),
      newRefTable.view.mapValues(t => memberMap.getOrElse(t, t)).toMap,
      db.globalTags,
      Nil
    )
  end cloneForAdoption
end SubDesignEntry

object SubDesignEntry:
  extension (entry: SubDesignEntry) def toJsonString: String = write(entry)
  def fromJsonString(json: String): SubDesignEntry = read[SubDesignEntry](json)
