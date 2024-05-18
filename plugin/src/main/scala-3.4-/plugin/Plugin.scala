package dfhdl.plugin

import dotty.tools.dotc.plugins.*

class Plugin extends StandardPlugin:
  val name: String = "dfhdl.plugin"
  override val description: String = "Dedicated DSL capabilities for DFiant HDL"

  def init(options: List[String]): List[PluginPhase] =
    val setting = new Setting(options.headOption)
    MetaContextPlacerPhase(setting) ::
      CustomControlPhase(setting) ::
      DesignDefsPhase(setting) ::
      MetaContextDelegatePhase(setting) ::
      MetaContextGenPhase(setting) ::
      OnCreateEventsPhase(setting) ::
      FixInterpDFValPhase(setting) ::
      Nil
end Plugin
