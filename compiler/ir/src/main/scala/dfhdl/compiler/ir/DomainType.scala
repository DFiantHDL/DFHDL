package dfhdl.compiler.ir

enum DomainType derives CanEqual:
  // dataflow domain
  case DF
  // register-transfer domain
  case RT(cfg: RTDomainCfg)
  // event-driven domain
  case ED
