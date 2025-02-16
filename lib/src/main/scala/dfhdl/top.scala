package dfhdl
import internals.AnnotatedWith

final case class top(genMain: Boolean = true)(using
    val elaborationOptions: dfhdl.options.ElaborationOptions,
    val compilerOptions: dfhdl.options.CompilerOptions,
    val printerOptions: dfhdl.options.PrinterOptions,
    val linterOptions: dfhdl.options.LinterOptions,
    val appOptions: dfhdl.options.AppOptions,
    annot: AnnotatedWith[top]
) extends scala.annotation.StaticAnnotation
