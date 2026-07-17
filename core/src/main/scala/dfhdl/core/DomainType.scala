package dfhdl.core

import dfhdl.compiler.ir
import scala.annotation.implicitNotFound

/** The timing model of the region the code is in, summoned as a given.
  *
  * A design or domain body supplies its own domain lexically (see `DomainContainer`), and a def
  * body supplies one through the context parameter its return modifier injects (`<> DFRET` gives
  * `DF`, `<> RTRET` gives `RT`, `<> EDRET` gives `ED`, `<> CONSTRET` gives `Static`).
  *
  * ~~~ Which domains are summonable where, and why it decides what a def can call ~~~
  *
  * A return modifier's context parameter must be summoned at the def's CALL SITE (the def's own
  * body is fine either way, since the context parameter supplies it there). So the givens below are
  * exactly what says where each kind of def may be called:
  *
  *   - `Static` is the AMBIENT given, so it is summonable everywhere, including where there is no
  *     domain at all. That makes the global scope static, which it always was in fact, and it makes
  *     a static function (`<> CONSTRET`) callable from every domain and from global scope alike.
  *   - `DF` is summonable in any DYNAMIC domain: lexically in a DF body, and through `fromRT` /
  *     `fromED` in an RT or ED body, because a dataflow design may be instantiated inside any of
  *     the three levels. It is deliberately NOT ambient: at global scope, and inside a static
  *     function body, there is nothing to instantiate a design into.
  *   - `RT` and `ED` are lexical only, which is what locks `<> RTRET` and `<> EDRET` to their own
  *     domain (an ED method's `DomainType.ED` is the whole reason it is ED-only).
  *
  * ~~~ The `Dynamic` layer ~~~
  *
  * `Dynamic` is a real layer, not a marker. Domain guards come in two forms, and the distinction
  * decides whether the static domain is admitted:
  *
  *   - a POSITIVE guard (`A <:< DomainType.RT`) rejects `Static` for free. `.REG`, `.SHARED`,
  *     `.din`, `.prev`, `.reg`, `:==`, and a sensitivity-list process are all of this form.
  *   - a NEGATIVE guard (`NotGiven[A <:< DomainType.DF]`) ADMITS `Static`, since static is not
  *     dataflow. Such a guard needs a positive `A <:< DomainType.Dynamic` conjunct if nothing else
  *     already rejects the construct in a static body. This is the type-level twin of an IR
  *     `case _ =>` fall-through swallowing `Static`. (The `Fork` and `Process` guards are of this
  *     form and are left alone on purpose: `Scope.Function` grants them no capability, and
  *     `DB.subprogramCheck` rejects them at elaboration regardless.)
  */
opaque type DomainType <: ir.DomainType = ir.DomainType

object DomainType:
  /** The three levels of abstraction: the domains in which values change over time. */
  opaque type Dynamic <: DomainType = DomainType

  opaque type DF <: Dynamic = Dynamic
  val DF: DF = ir.DomainType.DF
  // A dataflow design is instantiable inside any dynamic domain, so a `<> DFRET` def is callable
  // from an RT or ED body too. These are what make `DomainType.DF` summonable there without
  // making it ambient (which would also legalize the call at global scope, where there is no
  // design to instantiate into).
  given fromRT(using RT): DF = DF
  given fromED(using ED): DF = DF

  opaque type RT <: Dynamic = Dynamic
  val RT: RT = ir.DomainType.RT

  // summoned only at ED method (`<> EDRET`) call sites, hence the tailored message
  @implicitNotFound("An ED method can only be invoked inside an event-driven (ED) domain.")
  opaque type ED <: Dynamic = Dynamic
  val ED: ED = ir.DomainType.ED

  /** The degenerate bottom of the lattice: time does not advance, so every value is constant. Its
    * owners are the global scope and a static function's (`<> CONSTRET`) def design.
    *
    * This is the AMBIENT given: it is the one domain summonable with no domain in scope, so a bare
    * `DomainType` summon outside any design resolves here. That is what puts the global scope in
    * the static domain, and what makes a static function callable from everywhere.
    */
  opaque type Static <: DomainType = DomainType
  given Static: Static = ir.DomainType.Static

  extension (domainType: DomainType) def asIR: ir.DomainType = domainType
end DomainType
