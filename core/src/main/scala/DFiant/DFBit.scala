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

package DFiant

/**
  * A dataflow bit companion object.
  * Most inter-workings are relying on a DFBool
  */
object DFBit {
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Public Constructors
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  /**
    * Construct a new dataflow bit
    * @param ctx An implicit dataflow design context
    */
  def apply()(implicit ctx: DFAny.Context) =
    DFAny.NewVar(DFBool.Type(logical = false))
  def unapply(arg: DFAny.Member): Boolean =
    arg.dfType match {
      case DFBool.Type(logical) if !logical => true
      case _                                => false
    }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////

}
