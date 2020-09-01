package DFiant

import org.scalatest.flatspec.AnyFlatSpec
import internals.TopLevel

class DFTopSpec extends AnyFlatSpec {
  implicit val topLevel : TopLevel = new TopLevel
}
