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
  final protected implicit val cbOwner = CacheBox.Owner(self)
  protected[DFiant] trait __DevNameable {
    lazy val nameScala : String = "???"
    final private[Nameable] val nameManual : CacheBoxRW[String] = CacheBoxRW("")
    private[DFiant] var nameFirst : Boolean = false //hack
    private val nameAutoFunc : CacheBoxRW[Option[CacheBoxRO[String]]] = CacheBoxRW(None)
    final val nameTemp : CacheBoxRO[String] = CacheDerivedRO(nameManual, nameAutoFunc){
      if (!nameManual.isEmpty) nameManual
      else {
        val nameAuto : String = nameAutoFunc.map(x => x.unbox).getOrElse("")
        if (!nameAuto.isEmpty) nameAuto
        else nameScala
      }
    }
    final def setAutoName(name : => String) : self.type = {
      nameAutoFunc.set(Some(CacheBoxRO(name)))
      self
    }
    final def isAnonymous : Boolean = name.startsWith(Meta.Name.AnonStart) //|| isInstanceOf[DSLFoldableOwnerConstruct]
//    final def setAutoName[T](watch : => T, name : T => String) : self.type = {
//      nameAutoFunc.set(Some(StateDerivedRO(watch)(name)))
//      self
//    }
  }
  private[DFiant] lazy val __dev : __DevNameable = ???
  val name : CacheBoxRO[String]
  val fullName : CacheBoxRO[String]
  final def setName(name : String) : self.type = {__dev.nameManual.set(name); self}
  override def toString : String = name
}

trait TypeNameable {
  protected[DFiant] trait __DevTypeNameable {
    private var typeNameAuto : () => String = () => "???"
    lazy val typeName : String = typeNameAuto()
    final def setAutoTypeName(name : => String) : this.type = {typeNameAuto = () => name; this}
  }
  private[DFiant] lazy val __dev : __DevTypeNameable = new __DevTypeNameable {}
}

