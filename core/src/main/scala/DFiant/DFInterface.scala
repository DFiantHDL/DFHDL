package DFiant

import DFiant.internals._

import scala.collection.mutable.ListBuffer

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
  }
  override private[DFiant] lazy val __dev : __DevDFInterface = ???
  import __dev._
  override implicit def __theOwnerToBe : DFInterface = this

  final lazy val ports : List[DFAny.Port[DFAny, DFDir]] =
    memberList.collect{case o : DFAny => o}.filter(o => o.isPort).asInstanceOf[List[DFAny.Port[DFAny, DFDir]]]

  final lazy val portsIn : List[DFAny.Port[DFAny, IN]] =
    ports.filter(p => p.dir.isIn).map(p => p.asInstanceOf[DFAny.Port[DFAny, IN]])

  final lazy val portsOut : List[DFAny.Port[DFAny, OUT]] =
    ports.filter(p => p.dir.isOut).map(p => p.asInstanceOf[DFAny.Port[DFAny, OUT]])


  override def toString: String = s"$name : $typeName"
}

