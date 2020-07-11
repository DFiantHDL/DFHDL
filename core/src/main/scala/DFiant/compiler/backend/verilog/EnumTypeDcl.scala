package DFiant
package compiler.backend.verilog
import printer.formatter._

private object EnumTypeDcl {
  def apply(enumType: EnumType)(implicit printer : Printer) : String = {
    import printer.config._
    val typeList = enumType.entries.toList.sortBy(x => x._1).map {x =>
      val fullName = s"E_${enumType.name}_${x._2.name}".toUpperCase
      s"$FN`define $fullName ${ALGN(0)}${x._1}"
    }
    typeList.mkString("\n")
  }
}