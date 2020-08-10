package DFiant
package compiler.backend.vhdl

import DFiant.internals.BigIntExtras
import compiler.printer.formatter._

private object EnumTypeDcl {
  def constants(enumType: EnumType)(implicit printer : Printer) : String = {
    import printer.config._
    enumType.entries.toList.sortBy(x => x._1).map {x =>
      s"$KW constant ${enumEntryFullName(x._2)} ${ALGN(0)}: ${enumTypeName(enumType)} := ${uintRep(enumType, x._1)};"
    }.mkString("\n")
  }
  private def uintRep(enumType: EnumType, value : BigInt) : String = {
    s""""${value.toBitVector(enumType.width.getValue).toBin}""""
  }
  def enumTypeName(enumType: EnumType)(implicit printer : Printer) : String =
    s"E_${enumType.name}"
  def enumEntryFullName(entry : EnumType.Entry)(implicit printer: Printer) : String =
    s"${enumTypeName(entry.enumType)}_${entry.name}"
  object DefaultEncoding {
    def unapply(enumType : EnumType) : Boolean = enumType match {
      case auto: EnumType.Auto[_] if auto.encoding == EnumType.Encoding.Default => true
      case _ => false
    }
  }
  def tostrFuncName(enumType : EnumType)(implicit printer: Printer) : String = enumType match {
    case DefaultEncoding() => s"${enumTypeName(enumType)}'image"
    case _ => s"${enumTypeName(enumType)}_tostr"
  }
  private def tostrFuncHeader(enumType: EnumType)(implicit printer: Printer) : String = {
    import printer.config._
    s"$KW function ${tostrFuncName(enumType)}(e : ${enumTypeName(enumType)}) return string"
  }
  def body(enumType: EnumType)(implicit printer: Printer) : String = {
    import printer.config._
    enumType match {
      case DefaultEncoding() => ""
      case _ =>
        val caseItems : List[String] = enumType.entries.map {
          case (n, e) => Case.When(s"$LIT ${uintRep(enumType, n)}", List(s"""$KW return "${enumEntryFullName(e)}";"""))
        }.toList :+ Case.When(Case.Choice.Others(), List(s"""$KW return "XXXX";"""))
        s"""${tostrFuncHeader(enumType)} is
           |$KW begin
           |${Case("e", caseItems.mkString("\n"), allowDontCare = false).delim()}
           |$KW end ${tostrFuncName(enumType)};
           |""".stripMargin
    }
  }

  def apply(enumType: EnumType)(implicit printer : Printer) : String = {
    import printer.config._
    enumType match {
      case DefaultEncoding() =>
        val typeList = enumType.entries.toList.sortBy(x => x._1).map(x => EnumTypeDcl.enumEntryFullName(x._2))
        s"$KW type ${enumTypeName(enumType)} $KW is (${typeList.mkString(", ")});"
      case _ =>
        val uintType = Type(DFUInt.Type(enumType.width))
        val subtypeDcl = s"$KW subtype ${enumTypeName(enumType)} $KW is $uintType;"
        s"""$subtypeDcl
           |${constants(enumType)}
           |${tostrFuncHeader(enumType)};""".stripMargin
    }
  }
}