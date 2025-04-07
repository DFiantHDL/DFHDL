package dfhdl
import internals.AnnotatedWith

final case class top(genMain: Boolean = true)(using
    private[dfhdl] val annot: AnnotatedWith[top, dfhdl.core.Design]
)(using
    val elaborationOptions: dfhdl.options.ElaborationOptions.Defaults[annot.Out],
    val compilerOptions: dfhdl.options.CompilerOptions.Defaults[annot.Out],
    val printerOptions: dfhdl.options.PrinterOptions.Defaults[annot.Out],
    val linterOptions: dfhdl.options.LinterOptions.Defaults[annot.Out],
    val simulatorOptions: dfhdl.options.SimulatorOptions.Defaults[annot.Out],
    val appOptions: dfhdl.options.AppOptions.Defaults[annot.Out]
) extends scala.annotation.StaticAnnotation
