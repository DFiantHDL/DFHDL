package dfhdl.plugin

import dotty.tools.dotc.plugins.*
import dotty.tools.dotc.core.Contexts.Context

class Plugin extends StandardPlugin:
  val name: String = "dfhdl.plugin"
  override val description: String = "Dedicated DSL capabilities for DFiant HDL"

  override def initialize(options: List[String])(using Context): List[PluginPhase] =
    val setting = new Setting(options)
    val phases =
      PreTyperPhase(setting) ::
        TopAnnotPhase(setting) ::
        PureCheckPhase(setting) ::
        CodeDigestPhase(setting) ::
        ScalaVarPhase(setting) ::
        MetaContextPlacerPhase(setting) ::
        FlattenInlinedPhase(setting) ::
        LoopFSMPhase(setting) ::
        CustomControlPhase(setting) ::
        MethodsPhase(setting) ::
        MetaContextDelegatePhase(setting) ::
        MetaContextGenPhase(setting) ::
        OnCreateEventsPhase(setting) ::
        DesignClsSkipPhase(setting) ::
        Nil
    // The PluginErrCheck interceptor exists only for DFHDL's own test compilations, which
    // opt in via `-P:dfhdl.plugin:testing`; production compilations never pass the option,
    // so the phase is never instantiated there (see devdocs/plugin-error-testing.md).
    if (setting.testing) PluginTestPhase(setting) :: phases
    else phases
  end initialize
end Plugin
