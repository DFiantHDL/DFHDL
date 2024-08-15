package dfhdl.lib.mem
import dfhdl.*

class RTGenericRom[T <: DFType](val dataType: T, val depth: Int)(
    val romValues: (T X depth.type) <> CONST
) extends RTDesign:
  def this(dataType: T, depth: Int)(fillFunc: Int => (T <> CONST)) =
    this(dataType, depth)(
      // TODO: this should have just been `Vector.tabulate(depth)(fillFunc)`, but there seem
      // to be a scalac bug that sees difference between `T` of the main and auxiliary constructors.
      // Need to minimize and report.
      romValues = core.DFVector.Val.conv(dataType, depth)(Vector.tabulate(depth)(fillFunc))
    )

  val addr = Bits.until(depth) <> IN
  val data = dataType <> OUT
  val mem = dataType X depth <> VAR init romValues
  data := mem(addr).reg(1, init = ?)
end RTGenericRom
