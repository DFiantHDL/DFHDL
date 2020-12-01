package DFiant
package compiler.backend.vhdl
import compiler.printer.formatter._

private object Emitter {
  def unapply(emitter : BackendEmitter)(implicit printer: Printer) : Option[String] = emitter.backendStage match {
    case Backend =>
      import printer.config._
      val emit = emitter.seq.map {
        case Left(v) => Value.ref(v)
        case Right(s) => s
      }.mkString("")
      Some(emit)
    case _ => None
  }
}
