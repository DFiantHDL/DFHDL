package DFiant.internals

object errors {
  private final val docsURL = "https://dfianthdl.github.io/"
  private final val errorsURL = docsURL + "user-guide/errors/"
  object MissingContext {
    private final val url = errorsURL + "#missing-context"
    final val msg =
      "Missing an implicit DFDesign Context.\n" +
      "More details at " + url
  }

}
