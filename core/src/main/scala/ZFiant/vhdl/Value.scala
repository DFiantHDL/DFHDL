package ZFiant.vhdl

sealed trait Value extends Product with Serializable {
  val rtType : Value.Type
}

object Value {
  ////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Definition
  ////////////////////////////////////////////////////////////////////////////////////////////////////////
  case class Def(
    name : Name, rtType : Type, modifier : Def.Modifier, initStr : Option[String]
  ) extends Value with declaration
  object Def {
    sealed trait Modifier extends Product with Serializable
    object Modifier {
      case object signal extends Modifier
      case object variable extends Modifier
      sealed trait port extends Modifier
      object port {
        case object in extends port
        case object out extends port
      }
    }
  }
  ////////////////////////////////////////////////////////////////////////////////////////////////////////


  ////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Type
  ////////////////////////////////////////////////////////////////////////////////////////////////////////
  sealed trait Type extends Product with Serializable {
    val width : Int
  }
  protected object Type {
    ///////////////////////////////////////////////////////////
    // Capabilities
    ///////////////////////////////////////////////////////////
    sealed trait Resizeable extends Type {
      def resize(width : Int) : Type
    }
    sealed trait Invertable extends Type
    sealed trait Reverseable extends Type
    ///////////////////////////////////////////////////////////

    ///////////////////////////////////////////////////////////
    // std_logic_vector
    ///////////////////////////////////////////////////////////
    case class std_logic_vector(width : Int) extends Resizeable with Invertable with Reverseable {
      def resize(width : Int) : Type = std_logic_vector(width)
      override def toString: String = s"std_logic_vector(${width-1} downto 0)"
    }
    ///////////////////////////////////////////////////////////

    ///////////////////////////////////////////////////////////
    // unsigned
    ///////////////////////////////////////////////////////////
    case class unsigned(width : Int) extends Resizeable {
      def resize(width : Int) : Type = unsigned(width)
      override def toString: String = s"unsigned(${width-1} downto 0)"
    }
    ///////////////////////////////////////////////////////////

    ///////////////////////////////////////////////////////////
    // signed
    ///////////////////////////////////////////////////////////
    case class signed(width : Int) extends Resizeable {
      def resize(width : Int) : Type = signed(width)
      override def toString: String = s"signed(${width-1} downto 0)"
    }
    ///////////////////////////////////////////////////////////

    ///////////////////////////////////////////////////////////
    // std_logic
    ///////////////////////////////////////////////////////////
    case object std_logic extends Invertable {
      val width = 1
      override def toString: String = s"std_logic"
    }
    ///////////////////////////////////////////////////////////

    ///////////////////////////////////////////////////////////
    // boolean
    ///////////////////////////////////////////////////////////
    case object boolean extends Invertable {
      val width = 1
      override def toString: String = s"boolean"
    }
    ///////////////////////////////////////////////////////////

    //  case class enumeration(enum : Enum) extends Type {
    //    final val width = enum.width
    //    override def toString: String = db.Package.declarations.enums(enum).name.toString
    //  }
    //
    //  def apply(member : DFAny) : Type = member match {
    //    case x : DFBits[_] => std_logic_vector(x.width)
    //    case x : DFUInt[_] => unsigned(x.width)
    //    case x : DFSInt[_] => signed(x.width)
    //    case x : DFBool => std_logic
    //    case x : DFEnum[_] => enumeration(x.enum)
    //    case _ => throw new IllegalArgumentException(s"\nUnsupported type for VHDL compilation. The variable ${member.fullName} has type ${member.typeName}")
    //  }
    //  def apply(value : Value) : Type = value.typeS
  }
  ////////////////////////////////////////////////////////////////////////////////////////////////////////


  ////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Reference
  ////////////////////////////////////////////////////////////////////////////////////////////////////////
  sealed trait Reference extends Value {
    val refVal : Value
  }
  object Reference {
    case class resize(rtType : Type, refVal : Value) extends Reference
    object resize {
      def apply(refVal : Value, width : Int) : resize = refVal.rtType match {
        case t : Type.Resizeable => resize(t.resize(width), refVal)
        case _ => ???
      }
    }
    case class invert(refVal : Value) extends Reference {
      val rtType: Type = refVal.rtType
    }
  }
  ////////////////////////////////////////////////////////////////////////////////////////////////////////

}

