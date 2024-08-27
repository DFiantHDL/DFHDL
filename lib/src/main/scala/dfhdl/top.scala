package dfhdl

final case class top(genMain: Boolean = true)(using
    val elaborationOptions: dfhdl.options.ElaborationOptions,
    val compilerOptions: dfhdl.options.CompilerOptions,
    val printerOptions: dfhdl.options.PrinterOptions,
    val appOptions: dfhdl.options.AppOptions
) extends scala.annotation.StaticAnnotation
