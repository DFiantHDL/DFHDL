package Proc

import DFiant._

trait RegFile extends DFDesign {
  val rs1_addr  = DFBits(5)      <> IN
  val rs1_data  = DFBits(XLEN)   <> OUT
  val rs2_addr  = DFBits(5)      <> IN
  val rs2_data  = DFBits(XLEN)   <> OUT
  val rd_addr   = DFBits(5)      <> IN
  val rd_data   = DFBits(XLEN)   <> IN
  val rd_wren   = DFBool()       <> IN

  private val regsNum = 32

  private val regs = Array.fill(regsNum)(DFBits(XLEN).init(h"00000000"))
  regs.indices.foreach(i => {
    ifdf (rs1_addr.uint == i) {
      rs1_data := regs(i).prev
    }
    ifdf (rs2_addr.uint == i) {
      rs2_data := regs(i).prev
    }
    if (i > 0) { //Not writing to X0
      ifdf (rd_wren && (rd_addr.uint == i)) {
//        regs(i) := rd_data
      }
    }
  })

//  private val rs1_sel : DFBits[32] = regs.selectWith(rs1_addr.uint, h"00000000")
//  private val rs2_sel : DFBits[32] = regs.selectWith(rs2_addr.uint, h"00000000")
//  private val rd_sel : DFBits.Var[32] = regs.selectWith(rd_addr.uint, DFBits(XLEN))
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

//  val alu = new RegFile {}.printCodeString
  val aluTest = new RegFileTest {}.printCodeString
}