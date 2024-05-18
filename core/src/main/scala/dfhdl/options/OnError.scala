package dfhdl.options

enum OnError derives CanEqual:
  case Exit, Exception
object OnError:
  given OnError = OnError.Exit
