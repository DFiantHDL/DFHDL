package DFiant
package compiler.backend.vhdl

import DFiant.internals.BigIntExtras
import compiler.printer.formatter._

private object EnumEntriesDcl {
  def constants(entries: DFEnum.Entries)(implicit printer : Printer) : String = {
    import printer.config._
    entries.all.toList.sortBy(x => x._1).map {x =>
      s"$KW constant ${enumEntryFullName(x._2)} ${ALGN(0)}: ${entriesName(entries)} := ${uintRep(entries, x._1)};"
    }.mkString("\n")
  }
  private def uintRep(entries: DFEnum.Entries, value : BigInt) : String = {
    s""""${value.toBitVector(entries.width.getValue).toBin}""""
  }
  def entriesName(entries: DFEnum.Entries)(implicit printer : Printer) : String =
    s"E_${entries.name}"
  def enumEntryFullName(entry : DFEnum.Entries.Entry)(implicit printer: Printer) : String =
    s"${entriesName(entry.entries)}_${entry.name}"
  object DefaultEncoding {
    def unapply(entries : DFEnum.Entries) : Boolean = entries match {
      case auto: DFEnum.Auto[_] if auto.encoding == DFEnum.Encoding.Default => true
      case _ => false
    }
  }
  def tostrFuncName(entries : DFEnum.Entries)(implicit printer: Printer) : String = entries match {
    case DefaultEncoding() => s"${entriesName(entries)}'image"
    case _ => s"${entriesName(entries)}_tostr"
  }
  private def tostrFuncHeader(entries: DFEnum.Entries)(implicit printer: Printer) : String = {
    import printer.config._
    s"$KW function ${tostrFuncName(entries)}(e : ${entriesName(entries)}) return string"
  }
  def body(entries: DFEnum.Entries)(implicit printer: Printer) : String = {
    import printer.config._
    entries match {
      case DefaultEncoding() => ""
      case _ =>
        val caseItems : List[String] = entries.all.map {
          case (n, e) => Case.When(s"$LIT ${uintRep(entries, n)}", List(s"""$KW return "${enumEntryFullName(e)}";"""))
        }.toList :+ Case.When(Case.Choice.Others(), List(s"""$KW return "XXXX";"""))
        s"""${tostrFuncHeader(entries)} is
           |$KW begin
           |${Case("e", caseItems.mkString("\n"), allowDontCare = false).delim()}
           |$KW end ${tostrFuncName(entries)};
           |""".stripMargin
    }
  }

  def apply(entries: DFEnum.Entries)(implicit printer : Printer) : String = {
    import printer.config._
    entries match {
      case DefaultEncoding() =>
        val typeList = entries.all.toList.sortBy(x => x._1).map(x => EnumEntriesDcl.enumEntryFullName(x._2))
        s"""|$KW type ${entriesName(entries)} $KW is (
            |${typeList.mkString(",\n").delim()}
            |);""".stripMargin
      case _ =>
        val uintType = Type(DFUInt.Type(entries.width))
        val subtypeDcl = s"$KW subtype ${entriesName(entries)} $KW is $uintType;"
        s"""$subtypeDcl
           |${constants(entries)}
           |${tostrFuncHeader(entries)};""".stripMargin
    }
  }
}