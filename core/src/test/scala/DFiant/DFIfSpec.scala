import DFiant.*
import munit.*
import core.ifdf

class DFIfSpec extends DFSpec:
  val i = DFBool <> IN
  val x = DFUInt(8) <> VAR
  test("No ret val") {
    assertCodeString("") {
      def branch1: () => Unit = () => x := 1
      def branch2: () => Unit = () => x := 2
      val branches = List((i, branch1), (!i, branch2))
      ifdf.fromBranches[Unit](branches, None)
    }
  }
end DFIfSpec
