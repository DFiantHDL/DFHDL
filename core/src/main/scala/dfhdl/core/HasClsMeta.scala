package dfhdl.core

import dfhdl.compiler.ir

trait HasClsMeta:
  // The compiler plugin injects an `override` of this for every DFHDL class in
  // the inheritance chain, each prepending its own meta:
  //   override def __clsMeta = r__For_Plugin.metaGen(...) :: super.__clsMeta
  // so the result is the full chain, most-derived first (only concrete user
  // classes appear — abstract library bases are not processed). Containers build
  // their design block directly from this chain at creation, with no mutation:
  // the leaf (head) names the design/interface/resource, and for a blackbox IP
  // the base-most class in the chain names the IP type.
  protected def __clsMeta: List[ir.Meta] = Nil
end HasClsMeta

// Marks a container whose `<> CONST` constructor parameters are turned into
// design-parameter members (`DFVal.DesignParam`) by the compiler plugin —
// designs and interfaces. Other `HasClsMeta` classes (e.g. platform resources)
// may carry DFHDL-value parameters that must stay untouched.
trait HasClsArgs:
  // The (name, applied value) pairs of the `<> CONST` constructor parameters of the
  // most-derived class declaring such parameters, as applied at the instantiation site:
  //   override def __clsAppliedArgs =
  //     r__For_Plugin.clsAppliedArgs(List(("name", param), ...))
  // No chaining is needed: parameters of base classes (like auto-created parameters) are
  // recovered from their creation entries in the design context instead (see
  // `Design.Inst.collectParamEntries`). Used to construct the design instance's `paramMap`
  // at design end.
  protected def __clsAppliedArgs: List[(String, ir.DFVal)] = Nil
end HasClsArgs
