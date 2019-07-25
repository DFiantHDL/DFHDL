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
    lazy val nameScala : String = "???"
    private[internals] val nameManual : StateBoxRW[String]
    private[DFiant] var nameFirst : Boolean = false //hack
    private[internals] val nameAutoFunc : StateBoxRW[Option[StateBoxRO[String]]]
    final lazy val nameTemp : StateBoxRO[String] = StateDerivedRO((nameManual.get, nameAutoFunc.get)){_ =>
      if (!nameManual.isEmpty) nameManual
      else {
        val nameAuto : String = nameAutoFunc.map(x => x.get).getOrElse("")
        if (!nameAuto.isEmpty) nameAuto
        else nameScala
      }
    }
    val name : StateBoxRO[String]
    final def setAutoName(name : => String) : self.type = {
      nameAutoFunc.set(Some(StateConst(name)))
      self
    }
    final def setAutoName[T](watch : => T, name : T => String) : self.type = {
      nameAutoFunc.set(Some(StateDerivedRO(watch)(name)))
      self
    }
  }
  private[DFiant] lazy val __dev : __DevNameable = ???
  final def setName(name : String) : self.type = {__dev.nameManual.set(name); self}
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