package DFiant
package lib.ompss

import DFiant.internals._
import singleton.twoface.TwoFace
import singleton.ops._

/**
  * Ompss Array Partition Interface
  *
  * Construct using `OmpssARR()` apply method
  */
@df final class OmpssARR[AW] private (addrWidth : TwoFace.Int[AW], val factor : Int)(
  dir : Either[PortDir, INOUT.type]
) extends DFInterface {
  val partitions = Vector.tabulate(factor)(new OmpssARR.Partition(addrWidth, factor, _)(dir))

  /**
    * Disabling all partitions read/write.
    * @param dontCareAddress when true the address is assigned with a bubble (don't care)
    */
  @df def disable(dontCareAddress : Boolean = true) : Unit = partitions.foreach{p =>
    p.ce := 0
    dir match {
      case Left(IN) => //do nothing
      case _ =>
        p.we := 0
        p.d := ?
    }
    if (dontCareAddress) {
      p.address := ?
    }
  }

  /**
    * Sets the address for all partitions
    * @param addr the given address
    */
  @df def setAddress(addr : DFBits[Int]) : Unit = partitions.foreach{p =>
    p.ce := 1
    p.address := addr
  }
  @df def setAddress(addr : DFUInt[Int])(implicit di : DummyImplicit) : Unit = partitions.foreach{p =>
    require(addr.width <= p.address.width)
    p.ce := 1
    p.address := addr.resize(p.address.width).bits
  }
  /**
    * Reads from the selected partition
    * @param partSel the selected partition
    * @return the read data
    */
  @df def getDataRead(partSel : DFUInt[Int]) : DFBits[32] =
    partitions.map(_.q).selectdf(partSel)

  /**
    * Writes data to the selected partition
    * @param partSel the selected partition
    * @param data the written data
    */
  @df def write(partSel : DFUInt[Int], data : DFBits[32]) : Unit =
    partitions.foreachdf(partSel) {case p =>
      p.ce := 1
      p.we := 1
      p.d := data
    }
}

object OmpssARR {
  protected class PartitionName(idx : Int) extends DFOwner.NameFlatten {
    def apply(memberName : String, ownerName : String) : String = s"${idx}_${memberName}0"
  }

  /**
    * An Array Partition Interface
    */
  @df final class Partition[AW] private[ompss] (addrWidth : TwoFace.Int[AW], val factor : Int, val idx : Int)(
    dir : Either[PortDir, INOUT.type]
  ) extends DFInterface(new PartitionName(idx)) {
    lazy val address  = DFBits(addrWidth) <> OUT
    lazy val ce       = DFBit             <> OUT
    lazy val we       = DFBit             <> OUT
    lazy val d        = DFBits(32)        <> OUT
    lazy val q        = DFBits(32)        <> IN

    //touching only the relevant fields
    dir match {
      case Left(IN) => address; ce; q;
      case Left(OUT) => address; ce; we; d;
      case Right(INOUT) => address; ce; we; d; q;
    }
  }

  /**
    * A configuration node to construct the array partition from
    * @param addrWidth the address width
    * @param factor the parition factor
    */
  final class ConfigNode[AW] private[OmpssARR] (addrWidth : TwoFace.Int[AW], factor : Int) {
    def <> (dir : PortDir)(implicit ctx : ContextOf[OmpssARR[Nothing]]) : OmpssARR[AW] =
      new OmpssARR(addrWidth, factor)(Left(dir))
    def <> (dir : INOUT.type)(implicit ctx : ContextOf[OmpssARR[Nothing]]) : OmpssARR[AW] =
      new OmpssARR(addrWidth, factor)(Right(dir))
  }

  /**
    * Creates a configuration node to construct the array partition from
    * @param size size of the array
    * @param factor partitioning factor
    * @return a configuration node
    */
  def apply[S, F](
    size : Positive.Checked[S], factor : Positive.Checked[F]
  )(
    implicit addrWidth : BitsWidthOf.Int[(S / F) - 1]
  ) : ConfigNode[addrWidth.Out] = new ConfigNode(addrWidth(size / factor - 1), factor)
  protected object BlockFlatten extends DFOwner.NameFlatten {
    def apply(memberName : String, ownerName : String) : String =
      if (memberName == "offset") s"${ownerName}_$memberName" //the offset is special cases because there is no prefix here
      else s"m_axi_${ownerName}_$memberName" //Vivado adds "m_axi_" to the AXI interface signals
  }
}