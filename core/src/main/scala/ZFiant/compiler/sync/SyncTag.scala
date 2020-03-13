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

package ZFiant
package compiler.sync

sealed trait SyncTag extends DFMember.CustomTag
object SyncTag {
  case object Clk extends SyncTag {
    override def toString: String = "SyncTag.Clk"
  }
  case object Rst extends SyncTag {
    override def toString: String = "SyncTag.Rst"
  }
  case object Reg extends SyncTag {
    override def toString: String = "SyncTag.Reg"
  }
}
