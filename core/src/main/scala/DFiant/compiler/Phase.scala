package DFiant.compiler
import DFiant._

trait Phase {
  def apply(design : DFDesign) : DFDesign
}

case class PhaseSeq(seq : Seq[Phase]) extends Phase {
  def apply(design : DFDesign): DFDesign = seq.foldLeft(design)((dsn, phase) => phase(dsn))
}


object Phase {
  trait Flattener extends Phase

  trait ConstantPropagation extends Phase

  trait Printer extends Phase
}
