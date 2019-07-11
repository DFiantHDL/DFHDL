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

  object VarDFTypes {
    private final val url = errorsURL + "#dont-use-var-with-dataflow-valuesvariables"
    final val msg =
      "Don't use `var` with dataflow values/variables.\n" +
      "More details at " + url
    type Msg = msg.type
  }

}
