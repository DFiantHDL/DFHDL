package DFiant

import DFiant.internals._

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

    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Elaboration
    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    object externals {

      def isInheritedClass(parent: Any, child: Any): Boolean =
        if (parent == null || child == null) false
        else isInheritedClass(parent.getClass, child.getClass)

      def isInheritedClass(parent: Class[_], child: Class[_]): Boolean =
        if (parent == null || child == null) false
        else if (parent.isAssignableFrom(child)) { // is child or same class
          parent.isAssignableFrom(child.getSuperclass)
        }
        else false

      private def getDFVals(cls : Class[_]) : List[(DFAny, DFAny.Port[DFAny, DFDir])] =
        if (cls == null || cls == classOf[DFInterface]) List()
        else {
          val fields = cls.getDeclaredFields.toList
          fields.flatMap{f =>
            f.setAccessible(true)
            val ref = f.get(self)
            ref match {
              case ref : DFAny if ref != null && ref.owner != self =>
                val dir = if (f.getType.isAssignableFrom(classOf[DFAny.Var])) OUT else IN
                val port = ref.copyAsNewPort(dir).setName(f.getName).keep.asInstanceOf[DFAny.Port[DFAny, DFDir]]
                Some((ref, port))
              case _ => None
            }
          } ++ getDFVals(cls.getSuperclass)
        }

      lazy val named : Map[DFAny, DFAny.Port[DFAny, DFDir]] = getDFVals(self.getClass).toMap
    }
  }
  override private[DFiant] lazy val __dev : __DevDFInterface = ???
  import __dev._
  override implicit def __theOwnerToBe : DFInterface = this

  final lazy val ports : List[DFAny.Port[DFAny, DFDir]] =
    members.collect{case o : DFAny.Port[_,_] => o}.asInstanceOf[List[DFAny.Port[DFAny, DFDir]]]

  final lazy val portsIn : List[DFAny.Port[DFAny, IN]] =
    ports.filter(p => p.dir.isIn).map(p => p.asInstanceOf[DFAny.Port[DFAny, IN]])

  final lazy val portsOut : List[DFAny.Port[DFAny, OUT]] =
    ports.filter(p => p.dir.isOut).map(p => p.asInstanceOf[DFAny.Port[DFAny, OUT]])


  externals.named
  override def toString: String = s"$name : $typeName"
}

