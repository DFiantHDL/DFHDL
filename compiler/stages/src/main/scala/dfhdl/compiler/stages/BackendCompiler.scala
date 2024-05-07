package dfhdl.compiler.stages

import dfhdl.core.Design
import dfhdl.options.{CompilerOptions, PrinterOptions}
import dfhdl.compiler.printing.Printer
import dfhdl.compiler.ir.DB

trait BackendCompiler:
  final def compile[D <: Design](
      sd: StagedDesign[D]
  )(using co: CompilerOptions, po: PrinterOptions): CompiledDesign[D] =
    if (co.printDesignCodeBefore)
      println(
        """|~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
           |The design code before compilation:
           |~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~""".stripMargin
      )
      sd.printCodeString
    val ret = CompiledDesign(sd.newStage(printer(sd.stagedDB).printedDB))
    if (co.printDesignCodeAfter)
      println(
        """|~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
           |The design code after compilation:
           |~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~""".stripMargin
      )
      ret.printCodeString
    if (co.printGenFiles)
      println(
        """|~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
           |The generated backend code:
           |~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~""".stripMargin
      )
      ret.printGenFiles
    ret
  end compile
  def printer(
      designDB: DB
  )(using CompilerOptions, PrinterOptions): Printer
end BackendCompiler
