package DFiant.compiler
import DFiant.internals._

trait Phase {
//  def apply(almanac: Almanac): Almanac
}

//case class PhaseSeq(seq : Seq[Phase]) extends Phase {
//  def apply(almanac: Almanac): Almanac = seq.foldLeft(almanac)((alm, phase) => phase(alm))
//}


object Phase {
  trait Flattener extends Phase

  trait ConstantPropagation extends Phase

  trait Printer extends Phase
}
