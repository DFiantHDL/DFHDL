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

trait Nameable {self =>
  protected[DFiant] trait __DevNameable {
    protected def nameDefault : String = "???"
    private[Nameable] var nameManual : String = ""
    private var nameAutoFunc : () => String = () => ""
    private lazy val nameAuto : String = nameAutoFunc()
    final lazy val name : String = getUniqueName (
      if (!nameManual.isEmpty) nameManual
      else if (!nameAuto.isEmpty) nameAuto
      else nameDefault
    )
    private[internals] def getUniqueName(suggestedName : String) : String
    final def setAutoName(name : => String) : self.type = {nameAutoFunc = () => name; self}
  }
  private[DFiant] lazy val __dev : __DevNameable = ???
  final def setName(name : String) : self.type = {__dev.nameManual = name; self}
  override def toString : String = __dev.name
}

trait TypeNameable {
  protected[DFiant] trait __DevTypeNameable {
    private var typeNameAuto : String = "???"
    lazy val typeName : String = typeNameAuto
    final def setAutoTypeName(name : String) : this.type = {typeNameAuto = name; this}
  }
  private[DFiant] lazy val __dev : __DevTypeNameable = new __DevTypeNameable {}
}


trait NameIt {
  val value : String
}
object NameIt {
  import singleton.ops._
  type ForceNotVar[Sym] = RequireMsgSym[![ImplicitFound[sourcecode.IsVar]], errors.VarDFTypes.Msg, Sym]
  implicit def ev(implicit name : sourcecode.Name, ownerKind : sourcecode.OwnerKind)
  : NameIt = new NameIt {
    private val anonymous = ownerKind.value match {
      case sourcecode.OwnerKind.Lzy => false
      case sourcecode.OwnerKind.Val => false
      case sourcecode.OwnerKind.Var => false
      case sourcecode.OwnerKind.Obj => false
      case _ => true
    }
    lazy val value: String = {
      if (anonymous) s"${Name.AnonStart}anon" else name.value
    }
//    println(s"${name.value}, ${ownerKind.value}, $value")
  }
}

object Name {
  final val AnonStart : String = "dFt_"
  final val Separator : String = "_d_" //"ǂ"
}