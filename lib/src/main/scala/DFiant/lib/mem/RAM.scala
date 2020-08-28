package DFiant
package lib.mem
import internals._
import singleton.twoface._
import singleton.ops._
@df final class RAM[T <: DFAny.Type, W] (
  protected val cellTemplate : DFAny.NewVar[T], val cellNum : Int, addrWidth : TwoFace.Int[W]
) extends DFDesign {
//  protected implicit val __lateConstructionConfig : LateConstructionConfig = LateConstructionConfig.Force(false)
  object io {
    final val data_wr = DFBits(cellTemplate.width)  <> IN
    final val data_rd = DFBits(cellTemplate.width)  <> OUT
    final val addr    = DFBits(addrWidth)       <> IN
    final val wr_en   = DFBit()                 <> IN
  }
  io
  protected object vars {
    private val prefix = s"${owner.name}_"
    final lazy val addr       = DFBits(addrWidth) setNamePrefix(prefix) setNameSuffix("_var")
    final lazy val wr_en      = DFBit() setNamePrefix(prefix) setNameSuffix("_var")
    final lazy val data_wr    = DFAny.NewVar(cellTemplate.dfType) setNamePrefix(prefix) setNameSuffix("_var")
    lazy val connect : Unit = atOwnerDo {
      wr_en := 0
      addr := ?
      data_wr := ?
      (vars.addr <> io.addr) !! DFNet.LazyConnection
      (vars.wr_en <> io.wr_en) !! DFNet.LazyConnection
      (io.data_wr <> data_wr.bits) !! DFNet.LazyConnection
      cellTemplate <> io.data_rd.as(cellTemplate.dfType)
    }
  }
  protected class Port(addrAssignment : => Unit) extends DFAny.DefaultRet[T]{
    val dfType : T = cellTemplate.dfType
    private lazy val doAddrAssignment : Unit = addrAssignment
    def := [D](data : Exact[D])(implicit op : DFAny.`Op:=,<>`.Builder[T, D]) : Unit = {
      doAddrAssignment
      vars.wr_en := 1
      vars.data_wr := data
    }
    def thisVal(implicit ctx : DFAny.Context) : DFAny.Of[T] = {
      doAddrAssignment
      cellTemplate
    }
  }
  private var lastPortAccess : Option[(DFOwner, Any, Port)] = None
  def apply[I](cellIdx : Exact[I])(
    implicit
    ctx : DFNet.Context,
    op : DFAny.`Op:=,<>`.Builder[DFBits.Type[W], I]
  ) : Port = {
    vars.connect
    lastPortAccess match {
      case Some((lastOwner, lastIdx, port)) if lastOwner == ctx.owner && lastIdx == cellIdx.value => port //same port access
      case _ =>
        val port = new Port(vars.addr := cellIdx)
        lastPortAccess = Some((ctx.owner, cellIdx.value, port))
        port
    }
  }
}

object RAM {
  def apply[T <: DFAny.Type, N](cellTemplate : DFAny.NewVar[T], cellNum : Positive.Checked[N])(
    implicit ctx : ContextOf[RAM[_, _]], w : BitsWidthOf.Int[N - 1]
  ) : RAM[T, w.Out] = new RAM[T, w.Out](cellTemplate, cellNum, w(cellNum))
}