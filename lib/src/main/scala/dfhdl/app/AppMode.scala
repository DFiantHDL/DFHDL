package dfhdl.app

enum AppMode derives CanEqual:
  case help, elaborate, compile, commit, lint, simulate, build, program
