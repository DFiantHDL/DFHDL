package dfhdl.compiler.ir

enum InitFileFormat derives CanEqual:
  case Auto, VerilogBin, VerilogHex, AMDXilinxCOE, AMDXilinxMEM, IntelAlteraMIF, IntelAlteraHEX
