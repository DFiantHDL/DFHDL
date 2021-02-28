package DFiant
package compiler.backend.verilog

private object Type {
  def apply(dfType : DFAny.Type)(implicit printer : Printer) : String = {
    import printer.config._
    dfType match {
      case DFBits.Type(width) => s"[$LIT${width-1}:$LIT 0]"
      case DFUInt.Type(width) => s"[$LIT${width-1}:$LIT 0]"
      case DFSInt.Type(width) => s"$KW signed [$LIT${width-1}:$LIT 0]"
      case DFEnum.Type(entries) => s"[$LIT${entries.width-1}:$LIT 0]"
      case DFBool.Type(_) => ""
      case DFVector.Type(cellType, _) => Type(cellType) //the handling of array is done in Type.arrayDim
      case _ => throw new IllegalArgumentException(s"\nUnsupported type for Verilog compilation.\nFound type ${dfType}")
    }
  }
  def arrayDim(dfType : DFAny.Type)(implicit printer : Printer) : String = {
    import printer.config._
    dfType match {
      case DFVector.Type(_, cellNum) => s"[$LIT 0:$LIT${cellNum-1}]"
      case _ => ""
    }
  }}
