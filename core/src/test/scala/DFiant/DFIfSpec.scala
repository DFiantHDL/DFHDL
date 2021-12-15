import DFiant.*
import munit.*
import core.ifdf

class DFIfSpec extends DFSpec:
  val i = DFBool <> IN
  val x = DFUInt(8) <> VAR
  test("No ret val") {
    assertCodeString(
      """|if (i)
         |  x := d"8'1"
         |else if (!i)
         |  x := d"8'2"
         |""".stripMargin
    ) {
      def branch1: () => Unit = () => x := 1
      def branch2: () => Unit = () => x := 2
      val branches = List((i, branch1), (!i, branch2))
      ifdf.fromBranches[Unit](branches, None)
    }
    assertCodeString(
      """|if (i)
         |  x := d"8'1"
         |""".stripMargin
    ) {
      def branch1: () => Unit = () => x := 1
      val branches = List((i, branch1))
      ifdf.fromBranches[Unit](branches, None)
    }
    assertCodeString(
      """|if (i)
         |  x := d"8'1"
         |else if (!i)
         |  x := d"8'2"
         |  x := d"8'22"
         |else if (i || i)
         |  x := d"8'3"
         |""".stripMargin
    ) {
      def branch1: () => Unit = () => x := 1
      def branch2: () => Unit = () =>
        x := 2
        x := 22
      def branch3: () => Unit = () => x := 3
      val branches = List((i, branch1), (!i, branch2), (i || i, branch3))
      ifdf.fromBranches[Unit](branches, None)
    }
    assertCodeString(
      """|if (i)
         |  x := d"8'1"
         |else if (!i)
         |  x := d"8'2"
         |  x := d"8'22"
         |else if (i || i)
         |  x := d"8'3"
         |else
         |  x := d"8'7"
         |""".stripMargin
    ) {
      def branch1: () => Unit = () => x := 1
      def branch2: () => Unit = () =>
        x := 2
        x := 22
      def branch3: () => Unit = () => x := 3
      val branches = List((i, branch1), (!i, branch2), (i || i, branch3))
      ifdf.fromBranches[Unit](branches, Some(() => x := 7))
    }
    assertCodeString(
      """|if (i)
         |  x := d"8'1"
         |else if (!i)
         |  if (i)
         |    x := d"8'1"
         |  else
         |    x := d"8'7"
         |else if (i || i)
         |  x := d"8'3"
         |else
         |  x := d"8'7"
         |""".stripMargin
    ) {
      def branch1: () => Unit = () => x := 1
      def branch2: () => Unit = () =>
        val branches = List((i, branch1))
        ifdf.fromBranches[Unit](branches, Some(() => x := 7))
      def branch3: () => Unit = () => x := 3
      val branches = List((i, branch1), (!i, branch2), (i || i, branch3))
      ifdf.fromBranches[Unit](branches, Some(() => x := 7))
    }
  }
  test("With ret val") {
    assertCodeString(
      """|val res = 
         |if (i)
         |  d"8'1"
         |else if (!i)
         |  x.bits.uint
         |""".stripMargin
    ) {
      def branch1: () => DFUInt[8] <> VAL = () => 1
      def branch2: () => DFUInt[8] <> VAL = () => x.bits.uint
      val branches = List((i, branch1), (!i, branch2))
      val res = ifdf.fromBranches[DFUInt[8] <> VAL](branches, None)
    }
  }
end DFIfSpec
