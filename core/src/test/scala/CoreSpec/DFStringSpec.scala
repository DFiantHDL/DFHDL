package CoreSpec
import dfhdl.*
import munit.*

class DFStringSpec extends DFSpec:
  test("String Operations") {
    assertCodeString(
      """|val s1: String <> CONST = "Hello"
         |val s2: String <> CONST = "World"
         |val t1: String <> CONST = s1 + s2
         |val t2: String <> CONST = s1 + (" World")
         |val t4: Boolean <> CONST = s1 == s2
         |val t5: Boolean <> CONST = s1 != "Hello"
         |""".stripMargin
    ) {
      val s1: String <> CONST = "Hello"
      val s2: String <> CONST = "World"
      val t1 = s1 + s2
      val t2 = s1 + " World"
      val t3 = "Hello " + s2
      val t4 = s1 == s2
      val t5 = s1 != "Hello"
      assert(s1.toScalaString == "Hello")
      assert(t1.toScalaString == "HelloWorld")
      assert(t4.toScalaBoolean == false)
      assert(t5.toScalaBoolean == false)
    }
  }
end DFStringSpec
