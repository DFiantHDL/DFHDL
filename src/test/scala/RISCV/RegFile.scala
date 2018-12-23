package RISCV

import DFiant._

trait RegFile extends DFDesign {
  private val rs1_addr  = DFBits[5]      <> IN
  private val rs1_data  = DFBits[XLEN]   <> OUT
  private val rs2_addr  = DFBits[5]      <> IN
  private val rs2_data  = DFBits[XLEN]   <> OUT
  private val rd_addr   = DFBits[5]      <> IN
  private val rd_data   = DFBits[XLEN]   <> IN
  private val rd_wren   = DFBool()       <> IN

  private val regsNum = 0 until 32
  private val regs = regsNum.map(ri => (ri, DFBits[XLEN].init(b0s).setName(s"x$ri")))

  regs.foreachdf(rs1_addr) {case (ri, r) => rs1_data := r}
  regs.foreachdf(rs2_addr) {case (ri, r) => rs2_data := r}
  regs.foreachdf(rd_addr) {
    case (0, r) => //No write for X0
    case (ri, r) =>
      ifdf (rd_wren) {
        r := rd_data
      }
  }

  def readConn1(rs1_addr : DFBits[5])(implicit ctx : DFDesign.Context) : DFBits[XLEN] = {
    this.rs1_addr <> rs2_addr
    this.rs1_data
  }
  def readConn2(rs2_addr : DFBits[5])(implicit ctx : DFDesign.Context) : DFBits[XLEN] = {
    this.rs2_addr <> rs2_addr
    this.rs2_data
  }
  def writeConn(rd_addr : DFBits[5], rd_data : DFBits[XLEN], rd_wren : DFBool)(implicit ctx : DFDesign.Context) : Unit = {
    this.rd_addr <> rd_addr
    this.rd_data <> rd_data
    this.rd_wren <> rd_wren
  }
}




//  for(i <- 0 until 32) {
//    val r = regs(i).setName(s"reg$i")
//    ifdf (rs1_addr_u == i) {
//      rs1_data := r.prev
//    }
//    ifdf (rs2_addr_u == i) {
//      rs2_data := r.prev
//    }
//    if (i > 0) { //Not writing to X0
//      ifdf (rd_wren && (rd_addr_u == i)) {
//        r := rd_data
//      }
//    }
//  }




trait RegFileTest extends DFDesign {
}




object RegFileTestApp extends App {
  import Xilinx.FPGAs.`XC7VX485T-2FFG1761C`._
  implicit val a = DFAnyConfiguration.foldedInit

  val reg = new RegFile {}.printVHDLString
//  val regTest = new RegFileTest {}.printCodeString
}