package dfhdl.devices.tinytapeout
import dfhdl.*
import dfhdl.hw.unused

abstract class TT10 extends RTDesign:
  /** Dedicated inputs */
  val ui_in = Bits(8) <> IN

  /** Dedicated outputs */
  val uo_out = Bits(8) <> OUT

  /** IOs: Input path */
  val uio_in = Bits(8) <> IN

  /** IOs: Output path */
  val uio_out = Bits(8) <> OUT

  /** IOs: Enable path (active high: 0=input, 1=output) */
  val uio_oe = Bits(8) <> OUT

  /** Always 1 when the design is powered, so you can ignore it */
  @unused.quiet()
  val ena = Bit <> IN
end TT10

object TT10:
  // TODO: read from file
  lazy val configJSON: Map[String, String] = Map(
    "CLOCK_PORT" -> "clk",
    "CLOCK_PERIOD" -> "20"
  )
  lazy val defaultClkCfg = ClkCfg(
    edge = ClkCfg.Edge.Rising,
    rate = configJSON("CLOCK_PERIOD").toDouble.ns,
    portName = configJSON("CLOCK_PORT"),
    inclusionPolicy = ClkCfg.InclusionPolicy.AlwaysAtTop
  )
  lazy val defaultRstCfg = RstCfg(
    mode = RstCfg.Mode.Sync,
    active = RstCfg.Active.Low,
    portName = "rst_n",
    inclusionPolicy = RstCfg.InclusionPolicy.AlwaysAtTop
  )
  given options.ElaborationOptions.Defaults[TT10] =
    options.ElaborationOptions.defaults.copy(
      defaultClkCfg = defaultClkCfg,
      defaultRstCfg = defaultRstCfg
    )
end TT10
