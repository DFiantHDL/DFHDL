package DFiant

import DFiant.internals._

import scala.collection.mutable.ListBuffer

trait DFInterface extends DFAnyOwner { self =>
  trait __Dev extends super.__Dev {
    override lazy val typeName: String = {
      val cls = self.getClass
      val ifc = cls.getInterfaces
      if (ifc.isEmpty) { //No interfaces. This is a class
        if (cls.getSimpleName.contains("anon$")) cls.getSuperclass.getSimpleName //For anonymous classes we get the name of the superclass
        else cls.getSimpleName //get the name of the class
      } else {
        if (cls.getSimpleName.contains("anon$")) ifc.head.getSimpleName //get the name of the head interface
        else cls.getSimpleName
      }
    }
  }
  override val __dev : __Dev = new __Dev {}
  import __dev._
  override implicit def theOwnerToBe : DFInterface = this

  final lazy val ports : List[DFAny.Port[DFAny, DFDir]] =
    mutableMemberList.toList.collect{case o : DFAny => o}.filter(o => o.isPort).asInstanceOf[List[DFAny.Port[DFAny, DFDir]]]

  final lazy val portsIn : List[DFAny.Port[DFAny, IN]] =
    ports.filter(p => p.dir.isIn).map(p => p.asInstanceOf[DFAny.Port[DFAny, IN]])

  final lazy val portsOut : List[DFAny.Port[DFAny, OUT]] =
    ports.filter(p => p.dir.isOut).map(p => p.asInstanceOf[DFAny.Port[DFAny, OUT]])


  override def toString: String = s"$name : $typeName"
}

