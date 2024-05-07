import dfhdl._
object Extras:
  implicit class DFAnyExtras(reg: DFAny):
    def isValid: Bool = ???
import Extras._

@df class Proc extends DFDesign:
  val leds = Bits(8) <> OUT
  val rf = Bits(32).X(16) <> VAR init Vector(
    0, 1, 1, 0, 100, 0, 1
  ).padTo(16, 0).map(_.toBits(32))

  // A add, B blt
  val code = Bits(32).X(32) init Vector(
    h"32'0A556", // r5, r5, r6

    h"32'0A312", // r3 = r1 + r2
    h"32'0A120", // r1 = r2
    h"32'0A230", // r2 = r3
    h"32'0B034" // if r3 < r4: pc = 0
  ).padTo(32, h"32'0")

  object Insn extends DFStruct.Fields:
    val brtarget = UInt(16) <> VAL
    val opcode = Bits(4) <> VAL
    val rd, rs, rt = Bits(4) <> VAL

  val pc = UInt(32) <> VAR init 0
  val insn = Insn <> VAR // bubble init
  val wb_addr = Bits(4) <> VAR // bubble init
  val wb_data = Bits(32) <> VAR // bubble init

  insn := code(pc.bits(4, 0)).pipe.as(Insn).asInstanceOf[DFStruct[Insn.type]]
  val rs_data = rf(insn.rs).pipe
  val rt_data = rf(insn.rt).pipe
  val rs_data_fw = Bits(32).ifdf(insn.rs == wb_addr && wb_data.isValid) { wb_data }.elsedf {
    rs_data
  }
  val rt_data_fw = Bits(32).ifdf(insn.rt == wb_addr && wb_data.isValid) { wb_data }.elsedf {
    rt_data
  }

  ifdf(insn.opcode == h"A" && insn.rd != h"0") {
    wb_addr := insn.rd
  }.elsedf {
    wb_addr := ?
  }
  wb_data := rs_data_fw.uint + rt_data_fw.uint
  rf(wb_addr) := wb_data

  ifdf(insn.opcode == h"B" && rs_data_fw.uint < rt_data_fw.uint && insn.brtarget.isValid) {
    pc := insn.brtarget
    // flush by forcing bubbles in the pipeline
    insn := ?
    wb_data := ?
    wb_addr := ?
  }.elsedf {
    pc := pc + 1
  }

  if (sim.inSimulation)
    val cycle = UInt(32) <> VAR init 0
    ifdf(cycle >= 80) { sim.finish() }
    cycle := cycle + 1

    ifdf(wb_addr.isValid) {
      sim.report(msg"$cycle WB $pc:$insn $rs_data_fw,$rt_data_fw   $wb_data -> r$wb_addr")
    }.elsedf {
      sim.report(msg"$cycle WB $pc:$insn $rs_data_fw,$rt_data_fw")
    }
end Proc
