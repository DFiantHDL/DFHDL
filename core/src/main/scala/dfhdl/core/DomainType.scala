package dfhdl.core

import dfhdl.compiler.ir
import scala.annotation.implicitNotFound

/** The timing model of the region the code is in, summoned as a given.
  *
  * A design or domain body supplies its own domain lexically (see `DomainContainer`), and a def
  * body supplies one through the context parameter its return modifier injects (`<> DFRET` gives
  * `DF`, `<> EDRET` gives `ED`, `<> CONSTRET` gives `Static`). The AMBIENT given below is `Static`,
  * so code in no domain at all (the global scope) is static, which it always was in fact.
  *
  * `Dynamic` is a real layer, not a marker. Domain guards come in two forms, and the distinction
  * decides whether the static domain is admitted:
  *
  *   - a POSITIVE guard (`A <:< DomainType.RT`) rejects `Static` for free. `.REG`, `.SHARED`,
  *     `.din`, `.prev`, `.reg`, `:==`, and a sensitivity-list process are all of this form.
  *   - a NEGATIVE guard (`NotGiven[A <:< DomainType.DF]`) ADMITS `Static`, since static is not
  *     dataflow. Every such guard must therefore carry a positive `A <:< DomainType.Dynamic`
  *     conjunct, or it silently legalizes the construct inside a static function body. This is the
  *     type-level twin of an IR `case _ =>` fall-through swallowing `Static`.
  */
opaque type DomainType <: ir.DomainType = ir.DomainType

/** Holds the AMBIENT `DF` given, at lower priority than `object DomainType`'s ambient `Static`.
  *
  * `DF` must stay ambient, and the reason is the CALL SITE, not the body. `T <> DFRET` expands to
  * `(DFC, DomainType.DF) ?=> ...`, so applying such a def requires summoning `DomainType.DF` where
  * it is CALLED, and a `<> DFRET` def is callable from any domain and from the global scope. Inside
  * a DF design body the lexical `DomainContainer` given supplies it; everywhere else this ambient
  * one does. (The def's BODY does not need it: the context parameter provides it there.)
  *
  * It sits at low priority so that a bare `DomainType` summon in no domain at all resolves to
  * `Static` rather than being ambiguous, which is what puts the global scope in the static domain.
  * The two never compete otherwise: a `DomainType.DF` summon has only one candidate.
  */
sealed trait DomainTypeLP:
  given ambientDF: DomainType.DF = DomainType.DF

object DomainType extends DomainTypeLP:
  /** The three levels of abstraction: the domains in which values change over time. */
  opaque type Dynamic <: DomainType = DomainType

  opaque type DF <: Dynamic = Dynamic
  val DF: DF = ir.DomainType.DF

  opaque type RT <: Dynamic = Dynamic
  val RT: RT = ir.DomainType.RT

  // summoned only at ED method (`<> EDRET`) call sites, hence the tailored message
  @implicitNotFound("An ED method can only be invoked inside an event-driven (ED) domain.")
  opaque type ED <: Dynamic = Dynamic
  val ED: ED = ir.DomainType.ED

  /** The degenerate bottom of the lattice: time does not advance, so every value is constant. Its
    * owners are the global scope and a static function's (`<> CONSTRET`) def design.
    *
    * This is the AMBIENT given, which is what makes a static function callable from every domain
    * and from the global scope alike: its `DomainType.Static` context parameter is summonable
    * anywhere, unlike an ED method's `DomainType.ED`.
    */
  opaque type Static <: DomainType = DomainType
  given Static: Static = ir.DomainType.Static

  extension (domainType: DomainType) def asIR: ir.DomainType = domainType
end DomainType
