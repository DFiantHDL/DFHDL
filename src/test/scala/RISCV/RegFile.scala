package RISCV

import DFiant._

trait RegFile extends DFDesign {
  final val rs1_addr  = DFBits[5]      <> IN
  final val rs1_data  = DFBits[XLEN]   <> OUT
  final val rs2_addr  = DFBits[5]      <> IN
  final val rs2_data  = DFBits[XLEN]   <> OUT
  final val rd_addr   = DFBits[5]      <> IN
  final val rd_data   = DFBits[XLEN]   <> IN
  final val rd_wren   = DFBool()       <> IN

  private val regsNum = 32
  private val regs = List.fill(regsNum)(DFBits[XLEN].init(b0s))
  private val x0 = regs(0)
  private val rs1_addr_u = rs1_addr.uint
  private val rs2_addr_u = rs2_addr.uint
  private val rd_addr_u = rd_addr.uint

  private val rs1_sel = selectdf(rs1_addr_u)(regs)
  private val rs2_sel = selectdf(rs2_addr_u)(regs)
  private val rd_sel  = selectdf(rd_addr_u)(regs)
  rs1_data <> rs1_sel.prev
  rs2_data <> rs2_sel.prev
  ifdf (rd_wren) {
    rd_sel := rd_data
  }
  x0 := b0s //override X0 with zero value

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

  val reg = new RegFile {}.printCodeString
//  val regTest = new RegFileTest {}.printCodeString
}