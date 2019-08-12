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

package DFiant

import internals._

case class Comment(comment : String)(implicit ctx0 : DFDesign.Context) extends DFAnyMember {
  final private[DFiant] override lazy val ctx = ctx0
  protected[DFiant] trait __DevComment extends __DevDFAnyMember {
    def codeString : String = s"\n//$comment"

  }
  override private[DFiant] lazy val __dev : __DevComment = new __DevComment {}
  import __dev._
  keep
  id
}
