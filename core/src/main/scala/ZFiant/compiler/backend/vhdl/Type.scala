package ZFiant
package compiler.backend.vhdl

private object Type {
  def apply(member : DFAny)(implicit printer : Printer) : String = {
    import printer.config._
    member match {
      case DFBits(width) => s"$TP std_logic_vector($LIT${width-1} $KW downto $LIT 0)"
      case DFUInt(width) => s"$TP unsigned($LIT${width-1} $KW downto $LIT 0)"
      case DFSInt(width) => s"$TP signed($LIT${width-1} $KW downto $LIT 0)"
      case DFEnum(enumType) => s"${enumType.name}_type"
      case DFBit() => s"$TP std_logic"
      case DFBool() => s"$TP boolean"
      case _ => throw new IllegalArgumentException(s"\nUnsupported type for VHDL compilation. The variable ${member.getFullName} has type ${member.typeName}")
    }
  }
}
