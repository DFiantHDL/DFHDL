package DFiant
package compiler
package printer

trait Printer[C <: Printer.Config] {
  val getSet: MemberGetSet
  val config: C
  lazy val inSimulation: Boolean = getSet.designDB.top.simMode match {
    case sim.DFSimDesign.Mode.On => true
    case _                       => false
  }
}
object Printer {
  trait Config {
    val DELIM: String
    val maxAlignments: List[Int]
  }
  object Config {
    implicit def fromPrinter[C <: Printer.Config](implicit
        printer: Printer[C]
    ): Config = printer.config
  }
}
