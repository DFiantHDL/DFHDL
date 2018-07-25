package DFiant

import org.scalacheck._
import shapeless.test.illTyped
import TestUtils._
import psuedoVendor.family.device._

class ConnectTest extends Properties("ConnectTest") {
  trait IODesignConn1 extends DFDesign {
    val i = DFUInt(8) <> IN
    val o = DFUInt(8) <> OUT
    o <> i
  }

  property("DFDesign.codeString") = {
    val topIO = new DFDesign {
      val i = DFUInt(8) <> IN
      val o = DFUInt(8) <> OUT
      o <> i
    }
    topIO.codeString.trim ==
      """
        |val topIO = new DFDesign {
        |  val i = DFUInt(8) <> IN
        |  val o = DFUInt(8) <> OUT
        |  o <> i
        |}
      """.stripMargin.trim
  }

}