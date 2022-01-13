package DFiant.internals

trait HasTypeName:
  lazy val typeName: String =
    val cls = this.getClass
    val ifc = cls.getInterfaces
    val clsSimpleName = cls.getSimpleName
    val clsAnon = clsSimpleName.contains("anon$") || clsSimpleName.isEmpty
    val ret =
      if (ifc.isEmpty) // No interfaces. This is a class
        if (clsAnon)
          cls.getSuperclass.getSimpleName // For anonymous classes we get the name of the superclass
        else clsSimpleName // get the name of the class
      else if (clsAnon)
        ifc.head.getSimpleName // get the name of the head interface
      else clsSimpleName
    ret.replace("$", "") // removing $ from object names
