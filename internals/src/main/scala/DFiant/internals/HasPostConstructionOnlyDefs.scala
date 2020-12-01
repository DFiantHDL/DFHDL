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

trait PostConstruction
object PostConstruction {
  implicit def ev(implicit lp : shapeless.LowPriority) : PostConstruction = new PostConstruction {}
}

trait HasPostConstructionOnlyDefs {
  @scala.annotation.implicitAmbiguous("This is a post-construction definition only!")
  final protected implicit def __PostConstruction1(implicit lp : shapeless.LowPriority) : PostConstruction = new PostConstruction {}
  final protected implicit def __PostConstruction2(implicit lp : shapeless.LowPriority) : PostConstruction = new PostConstruction {}
}
