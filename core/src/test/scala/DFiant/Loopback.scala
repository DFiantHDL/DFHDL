package DFiant

import xilinx._
import compiler.backend.vhdl._
import compiler.sync._
trait Loopback extends VivadoHLSDesign {
  final val d         = new AXI4()
  final val d_offset  = DFBits(64) <> IN
  final val o         = new AXI4()
  final val o_offset  = DFBits(64) <> IN
  final val size      = DFBits(32) <> IN

  d.AR.notUsed
//  m_axi_d_AWADDR <= d_offset(32 - 1 downto 0);
//  m_axi_d_AWBURST <= ap_const_lv2_0;
//  m_axi_d_AWCACHE <= ap_const_lv4_0;
//  m_axi_d_AWID <= ap_const_lv1_0;
//  m_axi_d_AWLEN <= size;
//  m_axi_d_AWLOCK <= ap_const_lv2_0;
//  m_axi_d_AWPROT <= ap_const_lv3_0;
//  m_axi_d_AWQOS <= ap_const_lv4_0;
//  m_axi_d_AWREGION <= ap_const_lv4_0;
//  m_axi_d_AWSIZE <= ap_const_lv3_0;
//  m_axi_d_AWUSER <= ap_const_lv1_0;

}

object LoopbackApp extends App {
  object loopback_moved extends Loopback {
    this !! ClockParams("ap_clk", ClockParams.Edge.Rising)
    this !! ResetParams("ap_rst", ResetParams.Mode.Sync, ResetParams.Active.High)

  }
  loopback_moved.printCodeString().compile.toFolder("loopback")
}
