package dfhdl.plugin

import dotty.tools.dotc.plugins.*
import dotty.tools.dotc.core.Contexts.Context

class Plugin extends StandardPlugin:
  val name: String = "dfhdl.plugin"
  override val description: String = "Dedicated DSL capabilities for DFiant HDL"

  override def initialize(options: List[String])(using Context): List[PluginPhase] =
    val setting = new Setting(options.headOption)
    TopAnnotPhase(setting) ::
      MetaContextPlacerPhase(setting) ::
      CustomControlPhase(setting) ::
      DesignDefsPhase(setting) ::
      MetaContextDelegatePhase(setting) ::
      MetaContextGenPhase(setting) ::
      OnCreateEventsPhase(setting) ::
      FixInterpDFValPhase(setting) ::
      Nil
end Plugin
