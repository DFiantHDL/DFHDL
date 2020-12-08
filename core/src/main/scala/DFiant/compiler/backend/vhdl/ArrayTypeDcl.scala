package DFiant
package compiler.backend.vhdl

private object ArrayTypeDcl {
  def arrayTypeName(
      dfType: DFVector.Type[_ <: DFAny.Type, _]
  )(implicit printer: Printer): String = {
    s"${Type.typeName(dfType.cellType)}_arr${dfType.cellNum}"
  }
  def apply(
      dfType: DFVector.Type[_ <: DFAny.Type, _]
  )(implicit printer: Printer): String = {
    import printer.config._
    s"$KW type ${arrayTypeName(dfType)} $KW is $KW array($LIT 0 $KW to $LIT${dfType.cellNum - 1}) $KW of ${Type(dfType.cellType)};"
  }
}
