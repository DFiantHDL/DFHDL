package dfhdl.compiler.ir
import upickle.default.*

/** The timing model of a region.
  *
  * `Static` is the degenerate bottom of the lattice: a region in which time does not advance at
  * all, so every value in it is constant. The global scope has always been one without our having
  * said so, and a static function's def design is the other.
  *
  * `Dynamic` is a real sealed layer rather than a marker, and it is load-bearing: nearly every
  * existing `DomainType` site asks a *timing* question (does this owner need clk/rst, is `.prev` or
  * `.reg` the right history operator, is a process legal here), and such sites must keep matching
  * exhaustively over the three dynamic cases. A flat fourth case would instead let their `case _`
  * fall-throughs swallow `Static` silently, which is exactly the bug class to avoid.
  */
sealed trait DomainType extends HasRefCompare[DomainType] derives CanEqual, ReadWriter:
  protected def `prot_=~`(that: DomainType)(using MemberGetSet): Boolean =
    this == that

  lazy val getRefs: List[DFRef.TwoWayAny] = Nil

  def copyWithNewRefs(using RefGen): this.type = this
end DomainType

object DomainType:
  // static domain: time does not advance, so every value is constant
  case object Static extends DomainType
  // dynamic domains: the three levels of abstraction, where values change over time
  sealed trait Dynamic extends DomainType derives CanEqual, ReadWriter
  // dataflow domain
  case object DF extends Dynamic
  // register-transfer domain
  case object RT extends Dynamic
  // event-driven domain
  case object ED extends Dynamic
end DomainType
