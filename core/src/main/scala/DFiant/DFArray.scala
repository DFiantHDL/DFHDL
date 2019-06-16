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

//package DFiant
//
//import scala.collection.immutable._
//
//import singleton.twoface._
//sealed trait DFArray[E <: DFAny] extends DFAny.Val[WUnsafe, DFArray[E], DFArray.Var[E]] {
//  type TElem = E
//  type TAliasElem <: TElem#TVal
//
//  val length : Int
//  protected val dfVar : TElem#TVar
//
//  val width = length * dfVar.width.getValue
//  private val arr = Vector.tabulate[TAliasElem](length)(_ => dfVar.newEmptyDFVar.asInstanceOf[TAliasElem])
//  def apply(i : Int) : TAliasElem = arr(i)
//}
//
//
//object DFArray {
//  case class Var[E <: DFAny](length : Int, protected val dfVar : E#TVar) extends DFAny.Var[WUnsafe, DFArray[E], DFArray.Var[E]] with DFArray[E] {
//    type TAliasElem = TElem#TVar
//    def newEmptyDFVar = copy()
//  }
//
//  def apply(length : Int, dfVar : DFAny) = Var[dfVar.TVal](length, dfVar.asInstanceOf[dfVar.TVal#TVar])
//}
//
//
//
//object BB {
//  val a = DFArray(2,DFArray(5,DFBool()))
////  a(0)(0) := false
//}