package issues

import munit.*
import dfhdl.*

class IssuesSpec extends FunSuite:
  test("i116 compiles with no exception"):
    i116.GlobCounter(64).compile
