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

import DFiant.internals._

import scala.collection.immutable

trait DFInterface extends DFAnyOwner { self =>
  protected[DFiant] trait __DevDFInterface extends __DevDFAnyOwner {
    override lazy val typeName: String = {
      val cls = self.getClass
      val ifc = cls.getInterfaces
      val clsSimpleName = cls.getSimpleName
      val clsAnon = clsSimpleName.contains("anon$") || clsSimpleName.isEmpty
      if (ifc.isEmpty) { //No interfaces. This is a class
        if (clsAnon) cls.getSuperclass.getSimpleName //For anonymous classes we get the name of the superclass
        else clsSimpleName //get the name of the class
      } else {
        if (clsAnon) ifc.head.getSimpleName //get the name of the head interface
        else clsSimpleName
      }
    }
    protected lazy val discoveredOutputs : CacheBoxRO[List[DFAny.Port[DFAny, OUT]]] = ownerOption match {
      case Some(o : DFInterface) =>
        CacheDerivedRO(portsOut, o.__dev.discoveredSet)(portsOut.filter(o.__dev.discoveredSet.contains))
      case _ => portsOut
    }
    lazy val discoveredSet : CacheBoxRO[immutable.HashSet[DFAnyMember]] = ownerOption match {
      case Some(o : DFInterface) => o.__dev.discoveredSet
      case _ =>
        CacheDerivedRO(keepMembers, discoveredOutputs) {
          discover(immutable.HashSet(), discoveredOutputs ++ keepMembers)
        }
    }
  }
  override private[DFiant] lazy val __dev : __DevDFInterface = ???
  import __dev._
  override implicit def __theOwnerToBe : DFInterface = this

  final lazy val ports = CacheDerivedRO(addedMembers) {
    addedMembers.collect{case o : DFAny.Port[_,_] => o}.asInstanceOf[List[DFAny.Port[DFAny, DFDir]]]
  }

  final lazy val portsIn = CacheDerivedRO(ports) {
    ports.filter(p => p.dir.isIn).asInstanceOf[List[DFAny.Port[DFAny, IN]]]
  }

  final lazy val portsOut = CacheDerivedRO(ports) {
    ports.filter(p => p.dir.isOut).asInstanceOf[List[DFAny.Port[DFAny, OUT]]]
  }

  override def toString: String = s"$name : $typeName"
}

