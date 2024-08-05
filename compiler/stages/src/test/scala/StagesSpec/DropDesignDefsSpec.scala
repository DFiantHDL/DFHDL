package StagesSpec

import dfhdl.*
import dfhdl.compiler.stages.dropDesignDefs
// scalafmt: { align.tokens = [{code = "<>"}, {code = "="}, {code = "=>"}, {code = ":="}]}

class DropDesignDefsSpec extends StageSpec:
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
end DropDesignDefsSpec
