package DFiant.compiler.ir

enum DomainType derives CanEqual:
  // dataflow domain
  case DF
  // register-transfer domain
  case RT(clkCfg: ClkCfg, rstCfg: RstCfg)
  // event-driven domain
  case ED
