package issues

import munit.*
import dfhdl.*

class IssuesSpec extends FunSuite:
  test("i116 compiles with no exception"):
    i116.GlobCounter(64).compile
  test("i118 compiles and passes VHDL linting"):
    given options.CompilerOptions.Backend = backends.vhdl
    i118.ShiftIssue().compile.lint
  test("i126 compiles and passes VHDL linting"):
    given options.CompilerOptions.Backend = backends.vhdl
    i126.TypeConvertIssue().compile.lint
  test("i129 compiles and passes VHDL linting"):
    given options.CompilerOptions.Backend = backends.vhdl
    i129.StdLogicConvIssue().compile.lint
  test("i131 compiles with no exception"):
    i131.DictControl(fetch_count = 2).compile
  test("i133 compiles with no exception"):
    i133.Width0Issue(1).compile
end IssuesSpec
