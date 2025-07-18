package StagesSpec

import dfhdl.*
import dfhdl.compiler.stages.{explicitRomVar, dropUnreferencedAnons}
import dfhdl.options.CompilerOptions
import dfhdl.backends.verilog
// scalafmt: { align.tokens = [{code = "<>"}, {code = "="}, {code = "=>"}, {code = ":="}]}

class ExplicitRomVarSpec extends StageSpec:
  test("Basic constant vector ROM access") {
    given CompilerOptions.Backend = verilog.v95
    class ROMAccess extends DFDesign:
      val lookupTable: Bits[8] X 4 <> CONST = Vector(h"01", h"02", h"03", h"04")
      val index                             = UInt(2) <> IN
      val output                            = Bits(8) <> OUT
      output := lookupTable(index)
    val rom = (new ROMAccess).explicitRomVar
    assertCodeString(
      rom,
      """|class ROMAccess extends DFDesign:
         |  val lookupTable: Bits[8] X 4 <> CONST = DFVector(Bits(8) X 4)(h"01", h"02", h"03", h"04")
         |  val lookupTable_rom = Bits(8) X 4 <> VAR init lookupTable
         |  val index = UInt(2) <> IN
         |  val output = Bits(8) <> OUT
         |  output := lookupTable_rom(index.toInt)
         |end ROMAccess
         |""".stripMargin
    )
  }

  test("Multi-dimensional constant vector ROM access") {
    given CompilerOptions.Backend = verilog.v2001
    class MultiDimROM extends DFDesign:
      val lookupTable: Bits[8] X 4 X 4 <> CONST = Vector(
        Vector(h"01", h"02", h"03", h"04"),
        Vector(h"05", h"06", h"07", h"08"),
        Vector(h"09", h"10", h"11", h"12"),
        Vector(h"13", h"14", h"15", h"16")
      )
      val row    = UInt(2) <> IN
      val col    = UInt(2) <> IN
      val output = Bits(8) <> OUT
      output := lookupTable(row)(col)
    val rom = (new MultiDimROM).dropUnreferencedAnons.explicitRomVar
    assertCodeString(
      rom,
      """|class MultiDimROM extends DFDesign:
         |  val lookupTable: Bits[8] X 4 X 4 <> CONST =
         |    DFVector(Bits(8) X 4 X 4)(
         |      DFVector(Bits(8) X 4)(h"01", h"02", h"03", h"04"), DFVector(Bits(8) X 4)(h"05", h"06", h"07", h"08"),
         |      DFVector(Bits(8) X 4)(h"09", h"10", h"11", h"12"), DFVector(Bits(8) X 4)(h"13", h"14", h"15", h"16")
         |    )
         |  val lookupTable_rom = Bits(8) X 4 X 4 <> VAR init lookupTable
         |  val row = UInt(2) <> IN
         |  val col = UInt(2) <> IN
         |  val output = Bits(8) <> OUT
         |  output := lookupTable_rom(row.toInt)(col.toInt)
         |end MultiDimROM
         |""".stripMargin
    )
  }

  test("Multiple ROM accesses in same design") {
    given CompilerOptions.Backend = verilog.v2001
    class MultipleROMs extends DFDesign:
      val table1: Bits[8] X 4 <> CONST = Vector(h"01", h"02", h"03", h"04")
      val table2: Bits[8] X 4 <> CONST = Vector(h"AA", h"BB", h"CC", h"DD")
      val index1                       = UInt(2) <> IN
      val index2                       = UInt(2) <> IN
      val output1                      = Bits(8) <> OUT
      val output2                      = Bits(8) <> OUT
      val output3                      = Bits(8) <> OUT
      output1 := table1(index1)
      output2 := table2(index2)
      output3 := table1(index2)
    val rom = (new MultipleROMs).explicitRomVar
    assertCodeString(
      rom,
      """|class MultipleROMs extends DFDesign:
         |  val table1: Bits[8] X 4 <> CONST = DFVector(Bits(8) X 4)(h"01", h"02", h"03", h"04")
         |  val table1_rom = Bits(8) X 4 <> VAR init table1
         |  val table2: Bits[8] X 4 <> CONST = DFVector(Bits(8) X 4)(h"aa", h"bb", h"cc", h"dd")
         |  val table2_rom = Bits(8) X 4 <> VAR init table2
         |  val index1 = UInt(2) <> IN
         |  val index2 = UInt(2) <> IN
         |  val output1 = Bits(8) <> OUT
         |  val output2 = Bits(8) <> OUT
         |  val output3 = Bits(8) <> OUT
         |  output1 := table1_rom(index1.toInt)
         |  output2 := table2_rom(index2.toInt)
         |  output3 := table1_rom(index2.toInt)
         |end MultipleROMs
         |""".stripMargin
    )
  }

  test("ROM access in conditional block") {
    given CompilerOptions.Backend = verilog.v95
    class ConditionalROM extends DFDesign:
      val lookupTable: Bits[8] X 4 <> CONST = Vector(h"01", h"02", h"03", h"04")
      val index                             = UInt(2) <> IN
      val enable                            = Bit     <> IN
      val output                            = Bits(8) <> OUT
      if (enable)
        output := lookupTable(index)
      else
        output := h"00"
    val rom = (new ConditionalROM).explicitRomVar
    assertCodeString(
      rom,
      """|class ConditionalROM extends DFDesign:
         |  val lookupTable: Bits[8] X 4 <> CONST = DFVector(Bits(8) X 4)(h"01", h"02", h"03", h"04")
         |  val lookupTable_rom = Bits(8) X 4 <> VAR init lookupTable
         |  val index = UInt(2) <> IN
         |  val enable = Bit <> IN
         |  val output = Bits(8) <> OUT
         |  if (enable) output := lookupTable_rom(index.toInt)
         |  else output := h"00"
         |end ConditionalROM
         |""".stripMargin
    )
  }

  test("ROM access in process block") {
    given CompilerOptions.Backend = verilog.v2001
    class ProcessROM extends EDDesign:
      val lookupTable: Bits[8] X 4 <> CONST = Vector(h"01", h"02", h"03", h"04")
      val index                             = UInt(2) <> IN
      val output                            = Bits(8) <> OUT
      process(all) {
        output := lookupTable(index)
      }
    val rom = (new ProcessROM).explicitRomVar
    assertCodeString(
      rom,
      """|class ProcessROM extends EDDesign:
         |  val lookupTable: Bits[8] X 4 <> CONST = DFVector(Bits(8) X 4)(h"01", h"02", h"03", h"04")
         |  val lookupTable_rom = Bits(8) X 4 <> VAR init lookupTable
         |  val index = UInt(2) <> IN
         |  val output = Bits(8) <> OUT
         |  process(all):
         |    output := lookupTable_rom(index.toInt)
         |end ProcessROM
         |""".stripMargin
    )
  }

  test("No ROM creation for constant index access") {
    given CompilerOptions.Backend = verilog.v95
    class ConstantIndex extends DFDesign:
      val lookupTable: Bits[8] X 4 <> CONST = Vector(h"01", h"02", h"03", h"04")
      val output                            = Bits(8) <> OUT
      output := lookupTable(2) // Constant index, should not create ROM
    val rom = (new ConstantIndex).explicitRomVar
    assertCodeString(
      rom,
      """|class ConstantIndex extends DFDesign:
         |  val lookupTable: Bits[8] X 4 <> CONST = DFVector(Bits(8) X 4)(h"01", h"02", h"03", h"04")
         |  val output = Bits(8) <> OUT
         |  output := lookupTable(2)
         |end ConstantIndex
         |""".stripMargin
    )
  }

  test("No ROM creation for non-vector constants") {
    given CompilerOptions.Backend = verilog.v2001
    class NonVectorConstant extends DFDesign:
      val constant: Bits[8] <> CONST = h"AA"
      val index                      = UInt(3) <> IN
      val output                     = Bit     <> OUT
      output := constant(index) // Not a vector, should not create ROM
    val rom = (new NonVectorConstant).explicitRomVar
    assertCodeString(
      rom,
      """|class NonVectorConstant extends DFDesign:
         |  val constant: Bits[8] <> CONST = h"aa"
         |  val index = UInt(3) <> IN
         |  val output = Bit <> OUT
         |  output := constant(index.toInt)
         |end NonVectorConstant
         |""".stripMargin
    )
  }

  test("No ROM creation for non-constant vectors") {
    given CompilerOptions.Backend = verilog.v95
    class NonConstantVector extends DFDesign:
      val lookupTable = Bits(8) X 4 <> VAR init Vector(h"01", h"02", h"03", h"04")
      val index       = UInt(2)     <> IN
      val output      = Bits(8)     <> OUT
      output := lookupTable(index) // Not a constant, should not create ROM
    val rom = (new NonConstantVector).explicitRomVar
    assertCodeString(
      rom,
      """|class NonConstantVector extends DFDesign:
         |  val lookupTable = Bits(8) X 4 <> VAR init DFVector(Bits(8) X 4)(h"01", h"02", h"03", h"04")
         |  val index = UInt(2) <> IN
         |  val output = Bits(8) <> OUT
         |  output := lookupTable(index.toInt)
         |end NonConstantVector
         |""".stripMargin
    )
  }

  test("ROM creation only for supported backends") {
    given CompilerOptions.Backend = verilog.sv2005 // Not supported
    class UnsupportedBackend extends DFDesign:
      val lookupTable: Bits[8] X 4 <> CONST = Vector(h"01", h"02", h"03", h"04")
      val index                             = UInt(2) <> IN
      val output                            = Bits(8) <> OUT
      output := lookupTable(index) // Should not create ROM for sv2005
    val rom = (new UnsupportedBackend).explicitRomVar
    assertCodeString(
      rom,
      """|class UnsupportedBackend extends DFDesign:
         |  val lookupTable: Bits[8] X 4 <> CONST = DFVector(Bits(8) X 4)(h"01", h"02", h"03", h"04")
         |  val index = UInt(2) <> IN
         |  val output = Bits(8) <> OUT
         |  output := lookupTable(index.toInt)
         |end UnsupportedBackend
         |""".stripMargin
    )
  }

  test("ROM creation with complex vector types") {
    given CompilerOptions.Backend = verilog.v95
    case class ComplexType(x: Bits[8] <> VAL, y: Bits[8] <> VAL) extends Struct
    class ComplexROM extends DFDesign:
      val lookupTable: ComplexType X 4 <> CONST = Vector(
        ComplexType(h"01", h"02"),
        ComplexType(h"03", h"04"),
        ComplexType(h"05", h"06"),
        ComplexType(h"07", h"08")
      )
      val index  = UInt(2) <> IN
      val output = Bits(8) <> OUT
      output := lookupTable(index).x
    val rom = (new ComplexROM).dropUnreferencedAnons.explicitRomVar
    assertCodeString(
      rom,
      """|class ComplexROM extends DFDesign:
         |  final case class ComplexType(
         |      x: Bits[8] <> VAL
         |      y: Bits[8] <> VAL
         |  ) extends Struct
         |
         |  val lookupTable: ComplexType X 4 <> CONST =
         |    DFVector(ComplexType X 4)(
         |      ComplexType(x = h"01", y = h"02"), ComplexType(x = h"03", y = h"04"),
         |      ComplexType(x = h"05", y = h"06"), ComplexType(x = h"07", y = h"08")
         |    )
         |  val lookupTable_rom = ComplexType X 4 <> VAR init lookupTable
         |  val index = UInt(2) <> IN
         |  val output = Bits(8) <> OUT
         |  output := lookupTable_rom(index.toInt).x
         |end ComplexROM
         |""".stripMargin
    )
  }

  test("Global constant vector used in multiple internal designs") {
    given CompilerOptions.Backend         = verilog.v2001
    val globalTable: Bits[8] X 4 <> CONST = Vector(h"01", h"02", h"03", h"04")

    class InnerDesign1 extends DFDesign:
      val index  = UInt(2) <> IN
      val output = Bits(8) <> OUT
      output := globalTable(index)

    class InnerDesign2 extends DFDesign:
      val index  = UInt(2) <> IN
      val output = Bits(8) <> OUT
      output := globalTable(index)

    class TopDesign extends DFDesign:
      val index1  = UInt(2) <> IN
      val index2  = UInt(2) <> IN
      val output1 = Bits(8) <> OUT
      val output2 = Bits(8) <> OUT

      val inner1 = new InnerDesign1
      val inner2 = new InnerDesign2

      inner1.index  <> index1
      inner1.output <> output1
      inner2.index  <> index2
      inner2.output <> output2

    val rom = (new TopDesign).explicitRomVar
    assertCodeString(
      rom,
      """|val globalTable: Bits[8] X 4 <> CONST = DFVector(Bits(8) X 4)(h"01", h"02", h"03", h"04")
         |class InnerDesign1 extends DFDesign:
         |  val globalTable_rom = Bits(8) X 4 <> VAR init globalTable
         |  val index = UInt(2) <> IN
         |  val output = Bits(8) <> OUT
         |  output := globalTable_rom(index.toInt)
         |end InnerDesign1
         |
         |class InnerDesign2 extends DFDesign:
         |  val globalTable_rom = Bits(8) X 4 <> VAR init globalTable
         |  val index = UInt(2) <> IN
         |  val output = Bits(8) <> OUT
         |  output := globalTable_rom(index.toInt)
         |end InnerDesign2
         |
         |class TopDesign extends DFDesign:
         |  val index1 = UInt(2) <> IN
         |  val index2 = UInt(2) <> IN
         |  val output1 = Bits(8) <> OUT
         |  val output2 = Bits(8) <> OUT
         |  val inner1 = InnerDesign1()
         |  val inner2 = InnerDesign2()
         |  inner1.index <> index1
         |  output1 <> inner1.output
         |  inner2.index <> index2
         |  output2 <> inner2.output
         |end TopDesign
         |""".stripMargin
    )
  }

end ExplicitRomVarSpec
