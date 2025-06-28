package dfhdl.hw
import dfhdl.core.DFRange.ScalaRangesFlag
import dfhdl.core.TextOut.ScalaPrintsFlag
import dfhdl.core.TextOut.ScalaAssertsFlag
object flag:
  given scalaRanges: ScalaRangesFlag = new ScalaRangesFlag {}
  given scalaPrints: ScalaPrintsFlag = new ScalaPrintsFlag {}
  given scalaAsserts: ScalaAssertsFlag = new ScalaAssertsFlag {}
