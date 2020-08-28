package DFiant
package lib.mem
import internals._
import singleton.twoface._
import singleton.ops._
@df final class Mem[AAcs <: Mem.Access] (
  protected val aAcs : AAcs
) extends DFDesign {
  object io {
    final val portA = new Mem.Port(aAcs)
//    final lazy val portB = new Mem.Por
  }

  val portA = new Mem.__Port[AAcs](io.portA, aAcs)

  portA.connect
//  def apply[I](cellIdx : Exact[I])(
//    implicit
//    ctx : DFNet.Context,
//    op : DFAny.`Op:=,<>`.Builder[DFBits.Type[aAcs.TAddrWidth], I]
//  ) : Mem.__Port[AAcs] = portA(cellIdx)

}


object Mem {
  def apply[AAcs <: Access](aAcs : AAcs)(implicit ctx : ContextOf[Mem[Nothing]]) : Mem[AAcs] = new Mem(aAcs)
  //  def apply[AAcs <: Access, BAcs <: Access](aAcs : AAcs, bAcs : BAcs) : Unit = ???
//  def apply[AAcs <: Access, BAcs <: Access, CAcs <: Access](aAcs : AAcs, bAcs : BAcs, cAcs : CAcs) : Unit = ???

//  class SinglePortMemConstructor[AAcs <: Access](aAcs : AAcs)
//  object SinglePortMemConstructor {
//    final implicit class RWExt(c : SinglePortMemConstructor[RW.type]) {
//      def cells[T <: DFAny.Type, N](cellTemplate : DFAny.NewVar[T], cellNum : Positive.Checked[N])(
//        implicit ctx : ContextOf[Mem[_, _]], w : BitsWidthOf.Int[N - 1]
//      ) : Mem[T, w.Out] = new Mem[T, w.Out](cellTemplate, cellNum, w(cellNum))
//    }
//    final implicit class ROExt(c : SinglePortMemConstructor[RO.type]) {
//      def cells[T <: DFAny.Type, N](cellTemplate : DFAny.NewVar[T], cellNum : Positive.Checked[N])(
//        implicit ctx : ContextOf[Mem[_, _]], w : BitsWidthOf.Int[N - 1]
//      ) : Mem[T, w.Out] = new Mem[T, w.Out](cellTemplate, cellNum, w(cellNum))
//    }
//  }
//  class DualPortMemConstructor
//  class TriPortMemConstructor

  sealed trait PortSel
  trait ReadPortSel[R <: DFAny.Type] extends PortSel with DFAny.DefaultRet[R] {
    val readType : R
  }
  trait WritePortSel[W <: DFAny.Type] extends PortSel {
    val writeType : W
  }


  final protected class __Port[A <: Mem.Access](val portIO : Mem.Port[A], val acs : A)(
    implicit ctx : DFBlock.Context
  ) extends DFAny.DefaultRet[A#TReadType]{
    import DFDesign.Implicits._
    private val prefix = s"${ctx.owner.name}_"
    private lazy val en         = DFBit() setNamePrefix(prefix) setNameSuffix("_var")
    private lazy val addr       = DFBits(acs.addrWidth) setNamePrefix(prefix) setNameSuffix("_var")
    private lazy val data_rd    = DFBits(acs.readType.width) setNamePrefix(prefix) setNameSuffix("_var")
    private lazy val en_wr      = DFBit() setNamePrefix(prefix) setNameSuffix("_var")
    private lazy val data_wr    = DFBits(acs.writeType.width) setNamePrefix(prefix) setNameSuffix("_var")
    private[Mem] lazy val connect : Unit = ctx.atOwnerDo {
      addr := ?
      en := 0
      if (portIO.hasWrite) {
        en_wr := 0
        data_wr := ?
      }
      (en <> portIO.en) !! DFNet.LazyConnection
      (addr connectWith portIO.addr) !! DFNet.LazyConnection
      if (portIO.hasRead) {
        (data_rd connectWith portIO.data_rd) !! DFNet.LazyConnection
      }
      if (portIO.hasWrite) {
        (en_wr <> portIO.en_wr) !! DFNet.LazyConnection
        (portIO.data_wr <> data_wr) !! DFNet.LazyConnection
      }
    }

    val dfType : A#TReadType = acs.readType
    private var lastAccess : Option[(DFOwner, Any)] = None
    def apply[I](cellIdx : Exact[I])(
      implicit
      ctx : DFNet.Context,
      op : DFAny.`Op:=,<>`.Builder[DFBits.Type[acs.TAddrWidth], I]
    ) : this.type = {
      lastAccess match {
        case Some((lastOwner, lastIdx)) if lastOwner == ctx.owner && lastIdx == cellIdx.value => //do nothing
        case _ =>
          lastAccess = Some((ctx.owner, cellIdx.value))
          en := 1
          addr := cellIdx
      }
      this
    }
    protected[DFiant] def thisVal(implicit ctx : DFAny.Context) : DFAny.Of[A#TReadType] = {
      data_rd.as(acs.readType)
    }
  }
  object __Port {
    import DFDesign.Implicits._
    final implicit class Write[A <: Mem.WRITE](port : __Port[A]) {
      import port.acs
      def := [D](data : Exact[D])(implicit op : DFAny.`Op:=,<>`.Builder[acs.TWriteType, D], ctx : DFBlock.Context) : Unit = {
        port.en_wr := 1
        port.data_wr := op(acs.writeType, data).bits
      }
    }
  }

  sealed trait Access extends Product with Serializable {
    type TAddrWidth
    val addrWidth : TwoFace.Int[TAddrWidth]
    type TReadType <: DFAny.Type
    val readType : TReadType
    type TWriteType <: DFAny.Type
    val writeType : TWriteType
  }
  sealed trait READ extends Access
  sealed trait WRITE extends Access
  protected case object NA extends Access {
    type TAddrWidth = 0
    val addrWidth : TwoFace.Int[0] = 0
    type TReadType = Nothing
    val readType : TReadType = ???
    type TWriteType = Nothing
    val writeType : TWriteType = ???
  }
  final case class RO[AW, RT <: DFAny.Type](addrWidth : TwoFace.Int[AW], readType : RT) extends READ {
    type TAddrWidth = AW
    type TReadType = RT
    type TWriteType = Nothing
    val writeType : TWriteType = ???
  }
  //case object WO extends WRITE
  final case class RW[AW, RT <: DFAny.Type, WT <: DFAny.Type](
    addrWidth : TwoFace.Int[AW], readType : RT, writeType : WT
  ) extends READ with WRITE {
    type TAddrWidth = AW
    type TReadType = RT
    type TWriteType = WT
  }


  @df class Port[Acs <: Access](val acs: Acs) extends DFInterface {
    final val hasRead : Boolean = acs match {
      case _ : READ => true
      case _ => false
    }
    final val hasWrite : Boolean = acs match {
      case _ : WRITE => true
      case _ => false
    }

    final val en            = DFBit()                     <> IN
    final val addr          = DFBits(acs.addrWidth)       <> IN
    final lazy val data_rd  = DFBits(acs.readType.width)  <> OUT
    final lazy val en_wr    = DFBit()                     <> IN
    final lazy val data_wr  = DFBits(acs.writeType.width) <> IN

    //constructing only the necessary ios
    if (hasRead) data_rd
    if (hasWrite)  {
      en_wr
      data_wr
    }
  }
}