package DFiant.internals

import shapeless._

trait CaseClassMerge[T, U] {
  def apply(t: T, u: U): T
}

object CaseClassMerge {
  import ops.record.Merger

  def apply[T, U](implicit merge: CaseClassMerge[T, U]): CaseClassMerge[T, U] = merge

  implicit def mkCCMerge[T, U, RT <: HList, RU <: HList]
  (implicit
    tgen: LabelledGeneric.Aux[T, RT],
    ugen: LabelledGeneric.Aux[U, RU],
    merger: Merger.Aux[RT, RU, RT]
  ): CaseClassMerge[T, U] =
    new CaseClassMerge[T, U] {
      def apply(t: T, u: U): T =
        tgen.from(merger(tgen.to(t), ugen.to(u)))
    }
}
