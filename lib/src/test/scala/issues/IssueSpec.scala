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
