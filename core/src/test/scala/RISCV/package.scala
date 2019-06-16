/*
 *     This file is part of DFiant.
 *
 *     DFiant is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU Lesser General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     any later version.
 *
 *     DFiant is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     GNU Lesser General Public License for more details.
 *
 *     You should have received a copy of the GNU Lesser General Public License
 *     along with DFiant.  If not, see <https://www.gnu.org/licenses/>.
 */

import DFiant._

package object RISCV {
  final val XLEN = 32
  type XLEN = XLEN.type
  final val regsNum = 0 until 32

  sealed trait MicroArchitecture
  case object OneCycle extends MicroArchitecture
  case object TwoCycle extends MicroArchitecture

  val microArchitecture : MicroArchitecture = TwoCycle

  val caseIMem : Boolean = true //when true, implements the IMem for synthesis as a case statement
  val caseDMem : Boolean = true //when true, implements the DMem for synthesis as a case statement

  final val NOPInst = h"00000013"
  implicit object DebugOp extends Enum.Auto {
    val Unsupported, LUI, AUIPC, JAL, JALR, BEQ, BNE, BLT, BGE, BLTU, BGEU,
    LB, LH, LW, LBU, LHU, SB, SH, SW, ADDI, SLTI, SLTIU, XORI, ORI, ANDI,
    SLLI, SRLI, SRAI, ADD, SUB, SLL, SLT, SLTU, XOR, SRL, SRA, OR, AND,
    FENCE, FENCE_I, ECALL, EBREAK, CSRRW, CSRRS, CSRRC, CSRRWI, CSRRSI, CSRRCI = Entry()
  }
  type DebugOp = DebugOp.type

  implicit object BranchSel extends Enum.Manual(4) {
    val Next, BNE, BEQ, BGE,
    BGEU, BLT, BLTU, JAL, JALR = EntryDelta()
  }
  type BranchSel = BranchSel.type

  implicit object RS1OpSel extends Enum.Manual(2) {
    val RegSource, Immediate  = EntryDelta()
    val DontCare = RegSource //Giving another name to an entry as a Don't Care value
  }
  type RS1OpSel = RS1OpSel.type

  implicit object RS2OpSel extends Enum.Manual(2) {
    val RegSource, Immediate, PC  = EntryDelta()
    val DontCare = RegSource
  }
  type RS2OpSel = RS2OpSel.type

  implicit object ALUSel extends Enum.Manual(4) {
    val ADD, SUB, SLL, SRL, SRA, AND, OR, XOR, SLT, SLTU, COPY1 = EntryDelta()
    val DontCare = ADD
  }
  type ALUSel = ALUSel.type

  implicit object WriteBackSel extends Enum.Manual(2) {
    val ALU, Mem, PCPlus4, CSR = EntryDelta()
    val DontCare = ALU
  }
  type WriteBackSel = WriteBackSel.type

  implicit object DMemSel extends Enum.Auto {
    val LB, LH, LW, LBU, LHU, SB, SH, SW = Entry()
    val DontCare = LW
  }
  type DMemSel = DMemSel.type

}
