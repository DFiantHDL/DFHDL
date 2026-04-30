package StagesSpec

import dfhdl.*
import dfhdl.compiler.stages.dropDesignDefs
// scalafmt: { align.tokens = [{code = "<>"}, {code = "="}, {code = "=>"}, {code = ":="}]}

class DropDesignDefsSpec extends StageSpec(stageCreatesUnrefAnons = true):
  test("Design def") {
    class IDWithDesignDef extends DFDesign:
      val data = UInt(32) <> IN
      val o    = UInt(32) <> OUT

      /** This is my test
        * @param arg
        *   is a nice param
        * @return
        */
      def test(arg: UInt[32] <> VAL): UInt[32] <> DFRET =
        arg + arg
      o := test(data + 1)
      val x = test(data)
      o := x
    val id = (new IDWithDesignDef).dropDesignDefs
    assertCodeString(
      id,
      """|/** This is my test
         |  * @param arg
         |  *   is a nice param
         |  * @return
         |  **/
         |class test extends DFDesign:
         |  /**is a nice param*/
         |  val arg = UInt(32) <> IN
         |  val o = UInt(32) <> OUT
         |  o <> (arg + arg)
         |end test
         |
         |class IDWithDesignDef extends DFDesign:
         |  val data = UInt(32) <> IN
         |  val o = UInt(32) <> OUT
         |  val o_part_test_inst = test()
         |  o_part_test_inst.arg <> (data + d"32'1")
         |  o := o_part_test_inst.o
         |  val x = test()
         |  x.arg <> data
         |  o := x.o
         |end IDWithDesignDef
         |""".stripMargin
    )
  }
  test("Design def no return") {
    class IDWithDesignDef extends DFDesign:
      val data = UInt(32) <> IN
      val o    = UInt(32) <> OUT

      def test(arg: UInt[32] <> VAL): Unit <> DFRET =
        arg + arg
      test(data + 1)
      o := data
    val id = (new IDWithDesignDef).dropDesignDefs
    assertCodeString(
      id,
      """|class test extends DFDesign:
         |  val arg = UInt(32) <> IN
         |end test
         |
         |class IDWithDesignDef extends DFDesign:
         |  val data = UInt(32) <> IN
         |  val o = UInt(32) <> OUT
         |  val test_inst = test()
         |  test_inst.arg <> (data + d"32'1")
         |  o := data
         |end IDWithDesignDef
         |""".stripMargin
    )
  }
  test("Nested design def") {
    def sbox(lhs: Bits[8] <> VAL): Bits[8] <> DFRET = lhs

    def subWord(lhs: Bits[8] X 2 <> VAL): Bits[8] X 2 <> DFRET =
      DFVector(Bits(8) X 2)(sbox(lhs(0)), sbox(lhs(1)))
    end subWord

    class Foo extends DFDesign:
      val key = Bits(8) X 2 <> IN
      val o   = Bits(8) X 2 <> OUT
      o := subWord(key)

    val top = (new Foo).dropDesignDefs
    assertCodeString(
      top,
      """|class sbox extends DFDesign:
         |  val lhs = Bits(8) <> IN
         |  val o = Bits(8) <> OUT
         |  o <> lhs
         |end sbox
         |
         |class subWord extends DFDesign:
         |  val lhs = Bits(8) X 2 <> IN
         |  val o = Bits(8) X 2 <> OUT
         |  val o_part_sbox_inst = sbox()
         |  o_part_sbox_inst.lhs <> lhs(0)
         |  val o_part_sbox_inst = sbox()
         |  o_part_sbox_inst.lhs <> lhs(1)
         |  o <> DFVector(Bits(8) X 2)(o_part_sbox_inst.o, o_part_sbox_inst.o)
         |end subWord
         |
         |class Foo extends DFDesign:
         |  val key = Bits(8) X 2 <> IN
         |  val o = Bits(8) X 2 <> OUT
         |  val o_part_subWord_inst = subWord()
         |  o_part_subWord_inst.lhs <> key
         |  o := o_part_subWord_inst.o
         |end Foo""".stripMargin
    )
  }
end DropDesignDefsSpec
