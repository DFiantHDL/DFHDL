package dfhdl.hw
import dfhdl.core.DFRange.ScalaRangesFlag
import dfhdl.core.TextOut.ScalaPrintsFlag
import dfhdl.core.TextOut.ScalaAssertsFlag
import dfhdl.core.DFVal.ScalaBooleanFlag
object flag:
  given scalaRanges: ScalaRangesFlag = new ScalaRangesFlag {}
  given scalaPrints: ScalaPrintsFlag = new ScalaPrintsFlag {}
  given scalaAsserts: ScalaAssertsFlag = new ScalaAssertsFlag {}
  given scalaBoolean: ScalaBooleanFlag = new ScalaBooleanFlag {}
