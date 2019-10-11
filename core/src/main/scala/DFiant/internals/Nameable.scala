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
    val nameScala : String = "???"
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
    final def isAnonymous : Boolean = name.startsWith(Name.AnonStart) //|| isInstanceOf[DSLFoldableOwnerConstruct]
//    final def setAutoName[T](watch : => T, name : T => String) : self.type = {
//      nameAutoFunc.set(Some(StateDerivedRO(watch)(name)))
//      self
//    }
  }
  private[DFiant] lazy val __dev : __DevNameable = ???
  val name : CacheBoxRO[String]
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


trait Meta {
  val name : String
  val file : String
  val line : Int
  val column : Int
  val nameLine : Int //the line from which the name is fetched
  val nameColumn : Int //the column from which the name is fetched
}
object Meta {
  import singleton.ops._
  type ForceNotVar[Sym] = RequireMsgSym[![ImplicitFound[sourcecode.IsVar]], errors.VarDFTypes.Msg, Sym]
  implicit def ev(
    implicit
    nameSC : sourcecode.Name,
    ownerKind : sourcecode.OwnerKind,
    fileSC : sourcecode.File,
    lineSC : sourcecode.Line,
    columnSC : sourcecode.Column,
    nameLineSC : sourcecode.Name.Line,
    nameColumnSC : sourcecode.Name.Column
  ) : Meta = new Meta {
    private val anonymous = ownerKind.value match {
      case sourcecode.OwnerKind.Lzy => false
      case sourcecode.OwnerKind.Val => false
      case sourcecode.OwnerKind.Var => false
      case sourcecode.OwnerKind.Obj => false
      case sourcecode.OwnerKind.Def => false
      case _ => true
    }
    val name: String = {
      if (anonymous) s"${Name.AnonStart}anon" else nameSC.value
    }
    val file : String = fileSC.value
    val line : Int = lineSC.value
    val column : Int = columnSC.value
    val nameLine: Int = nameLineSC.value
    val nameColumn: Int = nameColumnSC.value

//    println(s"$name, ${ownerKind.value}, $file, $line, $column, $nameLine, $nameColumn")
  }
}

object Name {
  final val AnonStart : String = "dFt_"
  final val Separator : String = "_d_" //"ǂ"
}