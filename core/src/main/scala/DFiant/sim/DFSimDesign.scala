package DFiant
package sim

/**
  * The Basic Dataflow Simulation Design
  *
  * Every basic dataflow design simulation begins from extending this class.
  * This class is similar to [[DFDesign]], except that is also enables simulation constructs.
  * Without a `DFSimDesign` as top-level, all the simulation constructs at any level are not elaborated.
  * Additionally, a simulation design can be automatically run in a chosen simulator via [[SimulatorExt.simulation]].
  * @example
  * {{{
  *   @df class ID extends DFDesign {
  *     val i   = DFUInt(8) <> IN
  *     val o   = DFUInt(8) <> OUT
  *     o <> i
  *   }
  *   @df class IDTest extends sim.DFDimDesign {
  *     val id  = new ID
  *     val cnt = DFUInt(8) init 0
  *     id.i <> cnt
  *     sim.report(msg"$id.o") //will output the count value
  *     cnt := cnt + 1
  *   }
  * }}}
  *
  * @param ctx An implicit dataflow context for the design. Do not apply manually.
  */
abstract class DFSimDesign(implicit ctx: DFBlock.Context) extends DFDesign {
  final private[DFiant] override lazy val simMode: DFSimDesign.Mode =
    DFSimDesign.Mode.On
}

object DFSimDesign {
  private object Tag {
    final case class After(time: Int) extends DFMember.CustomTagOf[DFAny.Member]
  }

  sealed trait Mode extends Product with Serializable
  object Mode {
    case object Off extends Mode
    case object On  extends Mode
  }
}
