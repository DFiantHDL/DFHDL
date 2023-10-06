package StagesSpec

import dfhdl.*
import dfhdl.compiler.stages.dropDefDesigns
// scalafmt: { align.tokens = [{code = "<>"}, {code = "="}, {code = "=>"}, {code = ":="}]}

class DropDefDesignsSpec extends StageSpec:
  test("Design def") {
    class IDWithDesignDef extends DFDesign:
      val data = UInt(32) <> IN
      val o    = UInt(32) <> OUT

      /** This is my test
        * @param arg
        * @return
        */
      def test(arg: UInt[32] <> VAL): UInt[32] <> VAL =
        arg + arg
      o := test(data + 1)
      val x = test(data)
      o := x
    val id = (new IDWithDesignDef).dropDefDesigns
    assertCodeString(
      id,
      """|/** This is my test
         |  * @param arg
         |  * @return
         |  **/
         |class test extends RTDesign:
         |  val arg = UInt(32) <> IN
         |  val o = UInt(32) <> OUT
         |  o <> arg + arg
         |end test
         |
         |class IDWithDesignDef extends RTDesign:
         |  val data = UInt(32) <> IN
         |  val o = UInt(32) <> OUT
         |  val o_part_test_inst = test()
         |  o_part_test_inst.arg <> data + d"32'1"
         |  o := o_part_test_inst.o
         |  val x = test()
         |  x.arg <> data
         |  o := x.o
         |end IDWithDesignDef
         |""".stripMargin
    )
  }
end DropDefDesignsSpec
