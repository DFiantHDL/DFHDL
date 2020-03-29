package ZFiant
package compiler.backend.vhdl

private object Emitter {
  def unapply(emitter : BackendEmitter)(implicit printer: Printer) : Option[String] = {
    import printer.config._
    import formatter._
    val emit = emitter.seq.map {
      case Left(v) => Value.ref(v)
      case Right(s) => s
    }.mkString("")
    Some(emit)
  }
}
