package docExamples.uart_tx
import dfhdl.* //import all the DFHDL goodness

@top class UART_Tx(
    val CLK_FREQ_KHz: Int <> CONST = 50000,
    val BAUD_RATE_BPS: Int <> CONST = 115200
) extends RTDesign:
  val data_en = Bit <> IN
  val DATA_BITS = 8
  val data = Bits(DATA_BITS) <> IN
  val tx = Bit <> OUT.REG
  val tx_en = Bit <> OUT.REG
  val tx_done = Bit <> OUT.REG
  val BIT_CLOCKS = CLK_FREQ_KHz * 1000 / BAUD_RATE_BPS

  enum Status extends Encode.OneHot:
    case Idle, StartBit, DataBits, StopBit, Finalize
  import Status.*
  val status = Status <> VAR.REG init Idle
  val bitClkCnt = UInt.until(BIT_CLOCKS) <> VAR.REG init 0
  val dataBitCnt = UInt.until(DATA_BITS) <> VAR.REG init 0
  val shiftData = Bits(DATA_BITS) <> VAR.REG

  // To save on writing the "bit clock count wait" 3 times,
  // we use DFHDL's meta-programming capability.
  @internals.metaContextIgnore
  def waitBitAndThen(onThreshold: => Unit): Unit =
    if (bitClkCnt == BIT_CLOCKS - 1)
      bitClkCnt.din := 0
      onThreshold
    else bitClkCnt.din := bitClkCnt + 1

  status match
    case Idle =>
      tx_en.din := 0
      tx.din := 1
      tx_done.din := 0
      bitClkCnt.din := 0
      dataBitCnt.din := 0
      if (data_en)
        shiftData.din := data
        status.din := StartBit

    case StartBit =>
      tx_en.din := 1
      tx.din := 0
      waitBitAndThen { status.din := DataBits }

    case DataBits =>
      tx.din := shiftData.lsbit
      waitBitAndThen {
        shiftData.din := shiftData >> 1
        if (dataBitCnt == DATA_BITS - 1)
          dataBitCnt.din := 0
          status.din := StopBit
        else dataBitCnt.din := dataBitCnt + 1
      }

    case StopBit =>
      tx.din := 1
      waitBitAndThen {
        tx_done.din := 1
        status.din := Finalize
      }

    case Finalize =>
      tx_en.din := 0
      tx_done.din := 1
      status.din := Idle
  end match
end UART_Tx

////////////////////////////////////////////////////////////////////////////////////////////////
// DFHDL Elaboration Options:                                                                 //
////////////////////////////////////////////////////////////////////////////////////////////////
// Uncomment to set different clock and reset configurations:
// given options.ElaborationOptions.DefaultClkCfg = ClkCfg(ClkCfg.Edge.Rising)
// given options.ElaborationOptions.DefaultRstCfg = RstCfg(RstCfg.Mode.Async, RstCfg.Active.Low)
////////////////////////////////////////////////////////////////////////////////////////////////
// DFHDL Compiler Options:                                                                    //
////////////////////////////////////////////////////////////////////////////////////////////////
// Select backend compiler:
given options.CompilerOptions.Backend = backends.verilog
// Enables printing the generated chosen backend code:
given options.CompilerOptions.PrintGenFiles = true
// Uncomment to enable printing design code before compilation (after elaboration):
// given options.CompilerOptions.PrintDesignCodeBefore = true
// Uncomment to enable printing design code after compilation:
// given options.CompilerOptions.PrintDesignCodeAfter = true
////////////////////////////////////////////////////////////////////////////////////////////////
