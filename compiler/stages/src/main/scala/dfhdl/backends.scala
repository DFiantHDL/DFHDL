package dfhdl

import dfhdl.core.{Design, StagedDesign, BackendCompiler}
object backends:
  object verilog:
    given v2001: BackendCompiler = ???
    given sv2005: BackendCompiler = v2001
    given sv2012: BackendCompiler = sv2005
    given sv2017: BackendCompiler = sv2005

  object vhdl:
    given v93: BackendCompiler = ???
    given v2008: BackendCompiler = ???
    given v2019: BackendCompiler = v2008
