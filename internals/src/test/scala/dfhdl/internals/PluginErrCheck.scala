package dfhdl.internals

/** Compile-time testing hook for DFHDL plugin-phase diagnostics.
  *
  * Calls are intercepted by the plugin's PluginErrCheck phase (enabled via
  * `-P:dfhdl.plugin:testing`) and replaced with the literal list of error messages that compiling
  * `code` produces, including diagnostics emitted by the DFHDL plugin phases themselves, which
  * `compiletime.testing.typeCheckErrors` cannot surface. See devdocs/plugin-error-testing.md.
  *
  * This object lives in TEST sources on purpose: it is never published, and only test
  * configurations with a `test->test` dependency on internals can reference it.
  */
object PluginErrCheck:
  def pluginCheckErrors(code: String): List[String] =
    throw new IllegalStateException(
      "pluginCheckErrors must be replaced by the DFHDL plugin (enable -P:dfhdl.plugin:testing)"
    )
end PluginErrCheck
