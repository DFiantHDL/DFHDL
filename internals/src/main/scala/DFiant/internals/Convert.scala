//package dfhdl.internals
//
//trait ~[Of] extends Any:
//  type To <: Of
//  val value: To
//object ~ :
//  trait Conversion[Of, From]:
//    type To <: Of
//    def apply(from: From): To
//  given [Of, F <: Of]: Conversion[Of, F] with
//    type To = F
//    def apply(from: F): F = from
//  ///////////////////////////////////////////////////////////////////////////////////////////
//  // Reasons for workaround:
//  // 1. We cannot use transparent inline implicit conversion due to
//  //    https://github.com/lampepfl/dotty/issues/12429
//  // 2. We cannot directly summonInline the conversion
//  //    https://github.com/lampepfl/dotty/issues/12415
//  ///////////////////////////////////////////////////////////////////////////////////////////
//  protected class Aux[Of, To0 <: Of](val value: To0) extends AnyVal with ~[Of]:
//    type To = To0
//  implicit def getValue[Of, To <: Of](aux: Aux[Of, To]): To = aux.value
//  protected trait WorkAround[Of, From0]:
//    type To <: Of
//    type From = From0
//  protected trait LowPriority:
//    given gLP[Of, From]: WorkAround[Of, From] with
//      type To = Nothing
//  protected object WorkAround extends LowPriority:
//    given g[Of, From](using
//        c: Conversion[Of, From]
//    ): WorkAround[Of, From] with
//      type To = c.To
//  ///////////////////////////////////////////////////////////////////////////////////////////
//
//  inline implicit def conv[Of, From](inline from: From)(using
//      wa: WorkAround[Of, from.type]
//  ): Aux[Of, wa.To] =
//    val c = compiletime.summonInline[Conversion[Of, wa.From]]
//    new Aux[Of, wa.To](c(from).asInstanceOf[wa.To])
//end ~
