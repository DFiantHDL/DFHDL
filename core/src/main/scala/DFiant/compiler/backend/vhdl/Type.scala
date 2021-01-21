package DFiant
package compiler.backend.vhdl

private object Type {
  def apply(dfType: DFAny.Type)(implicit printer: Printer): String = {
    import printer.config._
    dfType match {
      case DFBits.Type(width) => s"$TP std_logic_vector($LIT${width-1} $KW downto $LIT 0)"
      case DFUInt.Type(width) => s"$TP unsigned($LIT${width-1} $KW downto $LIT 0)"
      case DFSInt.Type(width) => s"$TP signed($LIT${width-1} $KW downto $LIT 0)"
      case DFEnum.Type(entries) => EnumEntriesDcl.entriesName(entries)
      case DFBool.Type(false) => s"$TP std_logic"
      case DFBool.Type(true) => s"$TP boolean"
      case arr : DFVector.Type[_,_] => ArrayTypeDcl.arrayTypeName(arr)
      case _ => throw new IllegalArgumentException(s"\nUnsupported type for VHDL compilation. Found type ${dfType}")
    }
  }
  def typeName(dfType: DFAny.Type)(implicit printer: Printer): String = {
    dfType match {
      case DFBits.Type(width) => s"std_logic_vector$width"
      case DFUInt.Type(width) => s"unsigned$width"
      case DFSInt.Type(width) => s"signed$width"
      case DFEnum.Type(entries) => EnumEntriesDcl.entriesName(entries)
      case DFBool.Type(false) => "std_logic"
      case DFBool.Type(true) => "boolean"
      case arr : DFVector.Type[_,_] => ArrayTypeDcl.arrayTypeName(arr)
      case _ => throw new IllegalArgumentException(s"\nUnsupported type for VHDL compilation. Found type ${dfType}")
    }
  }
}
