package DFiant
import munit.*
import internals.{AllowTopLevel, HasTypeName}

class DFSpec extends FunSuite, AllowTopLevel, HasTypeName:
  final given dfc: DFC = core.DFC.empty
  private final val owner: core.DFOwner = core.DFDesign.Block(typeName)
  dfc.enterOwner(owner)
