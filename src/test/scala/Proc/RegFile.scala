package Proc

import DFiant._

trait RegFile extends DFDesign {
  final val rs1_addr  = DFBits(5)      <> IN
  final val rs1_data  = DFBits(XLEN)   <> OUT
  final val rs2_addr  = DFBits(5)      <> IN
  final val rs2_data  = DFBits(XLEN)   <> OUT
  final val rd_addr   = DFBits(5)      <> IN
  final val rd_data   = DFBits(XLEN)   <> IN
  final val rd_wren   = DFBool()       <> IN

  private val regsNum = 32
  private val regs = Array.fill(regsNum)(DFBits(XLEN).init(h"00000000"))
  private val rs1_addr_u = rs1_addr.uint
  private val rs2_addr_u = rs2_addr.uint
  private val rd_addr_u = rd_addr.uint

  regs.indices.foreach(i => {
    val r = regs(i).setName(s"reg$i")
    ifdf (rs1_addr_u == i) {
      rs1_data := r.prev
    }
    ifdf (rs2_addr_u == i) {
      rs2_data := r.prev
    }
    if (i > 0) { //Not writing to X0
      ifdf (rd_wren && (rd_addr_u == i)) {
        r := rd_data
      }
    }
  })

//  private val rs1_sel = regs.selectWith(rs1_addr_u)
//  private val rs2_sel = regs.selectWith(rs2_addr_u)
//  private val rd_sel  = regs.selectWith(rd_addr_u)
//  rs1_data <> rs1_sel.prev
//  rs2_data <> rs2_sel.prev
//  ifdf (rd_wren) {
//    rd_sel := rd_data
//  }



}





trait RegFileTest extends DFDesign {
}




object RegFileTestApp extends App {
  import Xilinx.FPGAs.`XC7VX485T-2FFG1761C`._
  implicit val a = DFAnyConfiguration.foldedInit

  val reg = new RegFile {}.printCodeString
//  val regTest = new RegFileTest {}.printCodeString
}