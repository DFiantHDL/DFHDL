package DFiant
package compiler.backend.verilog

private object Emitter {
  def unapply(emitter : BackendEmitter)(implicit printer: Printer) : Option[String] = emitter.backendStage match {
    case Backend =>
      val emit = emitter.seq.map {
        case Left(v) => Value.ref(v)
        case Right(s) => s
      }.mkString("")
      Some(emit)
    case _ => None
  }
}
