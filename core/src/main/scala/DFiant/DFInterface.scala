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

    lazy val discoveredSet : CacheBoxRO[Set[DFAnyMember]] = ownerOption match {
      case Some(o : DFInterface) => o.__dev.discoveredSet
      case _ =>
        CacheDerivedRO(membersChangeTracker) {
          val changeVersion = membersChangeTracker.unbox
          val ret = discover(Set(), portsOut)
          if (changeVersion != membersChangeTracker.unbox) discover(Set(), portsOut)
          else ret
        }
    }

    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Transparent Ports
    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    private lazy val anonCtx = ctx.asInstanceOf[DFAnyOwner.ContextOf[Any, DFBlock]].anonymize
    private lazy val portCtx = implicitly[DFAny.Port.Context].anonymize
    private def addTransparentPorts(cls : Class[_]) : List[(DFAny, DFAny.Port[DFAny, DFDir])] = {
      if (cls == null || cls == classOf[DFDesign] ||
        classOf[ConditionalBlock[_,_]].isAssignableFrom(cls) || classOf[DFFunc2[_,_,_]].isAssignableFrom(cls)) List()
      else {
        val fields = cls.getDeclaredFields.toList
        fields.flatMap{f =>
          f.setAccessible(true)
          val ref = f.get(self)
          ref match {
            case ref : DFAny if (ref ne null) && (ref.owner ne self) =>
              val dir = if (classOf[DFAny.Connectable[_]].isAssignableFrom(f.getType)) OUT else IN
              val port = ref.copyAsNewPort(dir)(portCtx).setName(f.getName).asInstanceOf[DFAny.Port[DFAny, DFDir]]
              dir match {
                case d : IN  => port.connectFrom(ref)(anonCtx)
                case d : OUT => ref.asInstanceOf[DFAny.Connectable[_]].connectFrom(port)(anonCtx)
              }
              Some((ref, port))
            case _ => None
          }
        } ++ addTransparentPorts(cls.getSuperclass)
      }
    }

    lazy val transparentPorts : Map[DFAny, DFAny.Port[DFAny, DFDir]] = addTransparentPorts(self.getClass).toMap
  }
  override private[DFiant] lazy val __dev : __DevDFInterface = ???
  import __dev._
  protected implicit def __interfaceOwner(implicit lp : shapeless.LowPriority) : DFInterface = this
  protected implicit val __replacementCtx : DFInterface.ReplacementContext = DFInterface.ReplacementContext(Some(this))

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

object DFInterface {
  implicit def fetchDev(from : DFInterface)(implicit devAccess: DevAccess) : from.__dev.type = from.__dev
  case class ReplacementContext(ownerOption : Option[DSLOwnerConstruct]) extends DSLContext
  object ReplacementContext {
    implicit def ev(implicit ctx : DSLContext) : ReplacementContext = ReplacementContext(Some(ctx.owner))
  }
}
