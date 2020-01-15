/*
 *     This file is part of DFiant.
 *
 *     DFiant is free software: you can redistribute it and/or modify
 *     it under the terms of the Lesser GNU General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     any later version.
 *
 *     DFiant is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     Lesser GNU General Public License for more details.
 *
 *     You should have received a copy of the Lesser GNU General Public License
 *     along with DFiant.  If not, see <https://www.gnu.org/licenses/>.
 */

package DFiant.internals

import scala.reflect.macros.whitebox
sealed trait CaseClassSkipper[T <: HasOut] extends HasOut {
  type Out
  def apply(value : T => Any, fallBack : => Boolean) : Out
}
object CaseClassSkipper {
  type Aux[T <: HasOut, Out0] = CaseClassSkipper[T]{type Out = Out0}
  //Normal value retrieval
  implicit def evNormal[T <: HasOut](implicit t : T) : Aux[T, t.Out] = new CaseClassSkipper[T] {
    type Out = t.Out
    def apply(value: T => Any, fallBack: => Boolean): t.Out = value(t).asInstanceOf[Out]
  }
  //Fallback value retrieval
  case class Fail[T <: HasOut]() extends CaseClassSkipper[T] {
    type Out = Boolean
    def apply(value: T => Any, fallBack: => Boolean): Boolean = fallBack
  }
  implicit def evCC[T <: HasOut](implicit n : shapeless.Refute[T]) : Aux[T, Boolean] = macro evCCMacro[T]
  def evCCMacro[T <: HasOut](c: whitebox.Context)(n : c.Tree)(implicit wt : c.WeakTypeTag[T]) : c.Tree = {
    import c.universe._
    val t = weakTypeOf[T]
    if (c.internal.enclosingOwner.owner.asClass.isCaseClass)
      q"DFiant.internals.CaseClassSkipper.Fail[$t]()"
    else
      c.abort(c.enclosingPosition, "Could not find implicit for...")
  }
}
