package DFiant.internals

trait ErrorMsg {
  protected final val docsURL = "https://dfianthdl.github.io/"
  protected final val errorsURL = docsURL + "user-guide/errors/"
  val msg : String with Singleton
}
