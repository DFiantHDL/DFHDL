package dfhdl.compiler
package analysis
import ir.*
import scala.collection.mutable

/** Which HDL-method blocks (ED methods / static functions) are emitted ONCE in the shared globals
  * area (a VHDL package / a Verilog defs header) instead of inlined in each using design.
  *
  * This is a PLACEMENT decision computed purely from the IR, and both the printers (which emit the
  * shared area) and `DropPackages` (which flattens the namespaces of everything placed there when
  * the backend has no packages) must agree on it, so it lives here as one definition rather than in
  * either of them. A backend may only WIDEN it (VHDL additionally globalizes a static function read
  * by a port declaration, since the entity is elaborated before the architecture) by overriding
  * `Printer.globalHDLMethods`.
  *
  * These read the design members directly, so they expect a FLAT DB (`newToOld`): the printers are
  * fed one, and a stage running on the hierarchical root must flatten first.
  */
extension (designDB: DB)
  /** The body members of an HDL-method block: the members it owns. */
  def methodBodyMembers(m: DFDesignBlock): List[DFMember] =
    designDB.designMemberTable.getOrElse(m, Nil)

  /** HDL-method blocks mapped to the set of NON-method designs that use them. A method call is
    * owned by the design (or method) whose body makes the call (`designBlockOwnershipMap`); a
    * method-to-method call is resolved transitively, so the resulting users are always real
    * designs.
    */
  private def hdlMethodDesignUsers: Map[DFDesignBlock, Set[DFDesignBlock]] =
    val ownership = designDB.designBlockOwnershipMap
    def realUsersOf(block: DFDesignBlock, seen: Set[DFDesignBlock]): Set[DFDesignBlock] =
      ownership.getOrElse(block, Set.empty).flatMap { owner =>
        if (!owner.isHDLMethod) Set(owner)
        else if (seen(owner)) Set.empty[DFDesignBlock]
        else realUsersOf(owner, seen + owner)
      }
    ownership.keysIterator.filter(_.isHDLMethod)
      .map(m => m -> realUsersOf(m, Set(m))).toMap

  /** An HDL method is emittable in a shared package/header only if its body references no value
    * captured from a single design. Captures materialize as PHANTOM input ports (globals are never
    * captured — they are reachable everywhere and referenced directly), so a method with any
    * phantom input is inherently design-local and stays inlined there.
    */
  def methodIsGlobalEligible(m: DFDesignBlock)(using MemberGetSet): Boolean =
    // every call of `m`, global-scope calls included (`members` covers the globals)
    def callSitesOf(m: DFDesignBlock): List[DFVal.Func] =
      designDB.members.collect {
        case DFVal.Func.Call(call, key) if key.getDesignBlock == m => call
      }
    val formals = designDB.methodBodyMembers(m).collect {
      case dcl: DFVal.Dcl if dcl.isPortIn => dcl
    }
    val phantomIdxs = formals.view.zipWithIndex.collect { case (f, i) if f.isPhantom => i }.toList
    // A capture materializes as a PHANTOM input port, whose actual is bound POSITIONALLY at
    // each call site. A GLOBAL actual is reachable from the shared package/header, so it keeps
    // the method eligible; a design-local one pins the method to its design. An actual that
    // cannot be lined up with the formals is treated as design-local (the conservative answer).
    phantomIdxs.isEmpty || callSitesOf(m).forall { call =>
      val actuals = call.args.map(_.get)
      actuals.length == formals.length && phantomIdxs.forall { i =>
        actuals(i) match
          case dfVal: DFVal.CanBeGlobal => dfVal.isGlobal
          case _                        => false
      }
    }
  end methodIsGlobalEligible

  /** Expand a set of HDL-method blocks to include everything they transitively call: an emitted
    * method's body calls them, and a shared package/header function cannot call one that is
    * declared inside a single design (or, for a method reached only from global scope, not declared
    * at all).
    */
  def methodCallClosure(seeds: Set[DFDesignBlock])(using MemberGetSet): Set[DFDesignBlock] =
    val result = mutable.Set.empty[DFDesignBlock]
    def visit(m: DFDesignBlock): Unit =
      if (result.add(m))
        designDB.methodBodyMembers(m).foreach {
          case DFVal.Func.Call(_, key) =>
            val callee = key.getDesignBlock
            if (callee.isHDLMethod) visit(callee)
          case _ =>
        }
    seeds.foreach(visit)
    result.toSet

  /** HDL-method blocks referenced by a GLOBAL `Func` call (a static function called at global
    * scope, e.g. to compute a global constant). Such a method has no design user, but must still be
    * emitted once in the shared globals area alongside the global value it computes.
    */
  private def globalCallMethods(using MemberGetSet): Set[DFDesignBlock] =
    designDB.membersGlobals.view.collect {
      case DFVal.Func.Call(_, key) => key.getDesignBlock
    }.filter(_.isHDLMethod).toSet

  /** HDL-method blocks emitted once in the shared globals area: used by more than one design, or
    * called from global scope; and package-eligible.
    */
  def globalHDLMethods(using MemberGetSet): Set[DFDesignBlock] =
    val byUsage = designDB.hdlMethodDesignUsers.iterator.collect {
      case (m, users) if users.sizeIs > 1 => m
    }
    designDB.methodCallClosure(byUsage.toSet ++ designDB.globalCallMethods)
      .filter(designDB.methodIsGlobalEligible)
end extension
