package ZFiant.compiler.backend.vhdl.syntax

final case class ValueDcl[+Mod <: ValueDcl.Modifier](modifier : Mod, name : String, rtType : String, initStrOption : Option[String]) extends Declaration {
  private val initStr = initStrOption match {
    case Some(i) => s" := $i"
    case None => ""
  }
  override def toString: String =
    s"${modifier.preModStr}$name : ${modifier.portDirStr}$rtType$initStr${modifier.postModStr}"
}
object ValueDcl {
  sealed trait Modifier extends Product with Serializable {
    val preModStr : String
    val postModStr : String
    val portDirStr : String
  }
  object Modifier {
    case object Signal extends Modifier {
      val preModStr : String = "signal "
      val postModStr : String = ";"
      val portDirStr : String = ""
    }
    case object Variable extends Modifier {
      val preModStr : String = "variable "
      val postModStr : String = ";"
      val portDirStr : String = ""
    }
    sealed trait Port extends Modifier {
      val preModStr : String = ""
      val postModStr : String = ""
    }
    object Port {
      case object In extends Port {
        val portDirStr : String = "in  "
      }
      case object Out extends Port {
        val portDirStr : String = "out "
      }
    }
  }
}
