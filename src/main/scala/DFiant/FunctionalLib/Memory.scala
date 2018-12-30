package DFiant.FunctionalLib

import DFiant._
import singleton.ops._
import singleton.twoface._
import internals._

//Memory
abstract class Memory[W, D] private (width : TwoFace.Int[W], depth : TwoFace.Int[D])(initContents : Array[BitVector] = Array())(
  implicit ctx : DFComponent.Context[Memory[W, D]]
) extends DFComponent[Memory[W, D]] {
  final val sizeBits = width * depth
  trait Port {
    type AW
    val addrWidth : TwoFace.Int[AW]
    final val addrToMem = new DFBits.NewVar[AW](addrWidth) <> IN
  }
  abstract class ReadPort[RW](readWidth : TwoFace.Int[RW]) extends Port {
    final val dataFromMem = new DFBits.NewVar[RW](readWidth) <> OUT
  }
  object ReadPort {
    type Aux[RW, AW0] = ReadPort[RW] {type AW = AW0}
    type Inference[RW] = BitsWidthOf.Int[(W * D) / RW]
    def apply[RW](readWidth: BitsWidth.Checked[RW])(implicit calcAddrWidth: Inference[RW])
    : Aux[RW, calcAddrWidth.Out] = new ReadPort[RW](readWidth) {
      type AW = calcAddrWidth.Out
      val addrWidth: TwoFace.Int[AW] = calcAddrWidth((width * depth) / readWidth)
    }
  }
  abstract class WritePort[WW](writeWidth : TwoFace.Int[WW]) extends Port {
    final val dataToMem = new DFBits.NewVar[WW](writeWidth) <> IN
    final val wrEnToMem = DFBool() <> IN
  }
  abstract class ReadWritePort(readWidth : Int, writeWidth : Int) extends ReadPort(readWidth) {
    final val dataToMem = DFBits(writeWidth) <> IN
    final val wrEnToMem = DFBool() <> IN
  }
}

object Memory {
  def rom[W, D](width : Int, depth : Int)(fillFunc : BitVector => BitVector) : Memory[W, D] = ???
//  def ro2[W, D](width : Int, depth : Int)

  /* TCL example
  create_ip -name blk_mem_gen -vendor xilinx.com -library ip -version 8.4 -module_name imem_bram
  set_property -dict [list CONFIG.Component_Name {imem_bram} CONFIG.Memory_Type {Single_Port_ROM} CONFIG.Write_Width_A {32} CONFIG.Write_Depth_A {4096} CONFIG.Read_Width_A {32} CONFIG.Enable_A {Always_Enabled} CONFIG.Write_Width_B {32} CONFIG.Read_Width_B {32} CONFIG.Register_PortA_Output_of_Memory_Primitives {false} CONFIG.Load_Init_File {true} CONFIG.Coe_File {/home/soronpo/XilinxProjects/ZedBoardEst/imem.coe} CONFIG.Fill_Remaining_Memory_Locations {true} CONFIG.Port_A_Write_Rate {0}] [get_ips imem_bram]

   */
}

