package DFiant
package compiler.backend.verilog

private object Type {
  def apply(member : DFAny)(implicit printer : Printer) : String = {
    import printer.config._
    member match {
      case DFBits(width) => s"[$LIT${width-1}:$LIT 0]"
      case DFUInt(width) => s"[$LIT${width-1}:$LIT 0]"
      case DFSInt(width) => s"$KW signed [$LIT${width-1}:$LIT 0]"
      case DFEnum(enumType) => s"[$LIT${enumType.width-1}:$LIT 0]"
      case DFBit() => s""
      case DFBool() => s""
      case _ => throw new IllegalArgumentException(s"\nUnsupported type for Verilog compilation. The variable ${member.getFullName} has type ${member.typeName}")
    }
  }
}
