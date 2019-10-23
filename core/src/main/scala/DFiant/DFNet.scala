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
import internals._

sealed abstract class DFNet(netSymbol : String, netName : String)(implicit ctx0 : DFNet.Context) extends DFAnyMember {
  final private[DFiant] override lazy val ctx = ctx0
  val toVal : DFAny
  val fromVal : DFAny
  protected[DFiant] trait __DevDFNet extends __DevDFAnyMember {
    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Naming
    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    override lazy val nameScala = s"${Meta.Name.Separator}$netName"
    def codeString : String = s"\n${toVal.refCodeString} $netSymbol ${fromVal.refCodeString}"
  }
  override private[DFiant] lazy val __dev : __DevDFNet = new __DevDFNet {}
  import __dev._
  id
}

object DFNet {
  type Context = DFAnyOwner.Context[DFBlock]
  final case class Connection(toVal : DFAny, fromVal : DFAny)(implicit ctx0 : Context) extends DFNet("<>", "connect") {
    protected[DFiant] trait __DevConnection extends __DevDFNet {
      /////////////////////////////////////////////////////////////////////////////////////////////////////////
      // Naming
      /////////////////////////////////////////////////////////////////////////////////////////////////////////
      override def codeString : String = toVal.owner match {
        case f : DSLSelfConnectedFoldableOwnerConstruct if f.isFolded => ""
        case _ => super.codeString
      }
    }
    override private[DFiant] lazy val __dev : __DevConnection = new __DevConnection {}
  }

  final case class Assignment(toVal : DFAny, fromVal : DFAny)(implicit ctx0 : Context) extends DFNet(":=", "assign")
}

