package DFiant.internals

object errors {
  final object MissingContext extends ErrorMsg (
    "Missing an implicit DFDesign Context.",
    "missing-context"
  ) {
    final val msg = getMsg
  }

  final object VarDFTypes extends ErrorMsg (
    "Don't use `var` with dataflow values/variables.",
    "dont-use-var-with-dataflow-valuesvariables"
  )

}
