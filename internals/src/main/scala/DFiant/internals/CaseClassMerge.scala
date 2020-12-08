/*
 *     This file is part of DFiant.
 *
 *     DFiant is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU Lesser General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     any later version.
 *
 *     DFiant is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     GNU Lesser General Public License for more details.
 *
 *     You should have received a copy of the GNU Lesser General Public License
 *     along with DFiant.  If not, see <https://www.gnu.org/licenses/>.
 */

package DFiant.internals

import shapeless._

trait CaseClassMerge[T, U] {
  def apply(t: T, u: U): T
}

object CaseClassMerge {
  import ops.record.Merger

  def apply[T, U](implicit merge: CaseClassMerge[T, U]): CaseClassMerge[T, U] =
    merge

  implicit def mkCCMerge[T, U, RT <: HList, RU <: HList](implicit
      tgen: LabelledGeneric.Aux[T, RT],
      ugen: LabelledGeneric.Aux[U, RU],
      merger: Merger.Aux[RT, RU, RT]
  ): CaseClassMerge[T, U] =
    new CaseClassMerge[T, U] {
      def apply(t: T, u: U): T =
        tgen.from(merger(tgen.to(t), ugen.to(u)))
    }
}
