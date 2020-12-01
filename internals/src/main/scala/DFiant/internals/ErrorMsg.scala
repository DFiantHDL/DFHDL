package DFiant.internals

import singleton.ops._
object DocURLs {
  protected final val docsURL = "https://dfianthdl.github.io/"
  protected final val errorsURL = docsURL + "user-guide/errors/#"
  type ErrorsURL = errorsURL.type
}
class ErrorMsg[M <: String with Singleton, P <: String with Singleton](specificMsg : M, place : P) {
  type URL = DocURLs.ErrorsURL + P
  type Msg = M + "\nMore details at " + URL
  final def getMsg(implicit m : SafeString[Msg]) : m.Out = m.value
}
