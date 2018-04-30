package DFiant.compiler
import DFiant.internals._

trait Phase {
  def apply(almanac: Almanac): Almanac = ???
  def -> (phase : Phase) : PhaseSeq = ???
}

trait PhaseSeq extends Phase

trait Flattener extends Phase

trait ConstantPropagation extends Phase
