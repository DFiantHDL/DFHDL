package DFiant.plugin

import dotty.tools.dotc.plugins._

class Plugin extends StandardPlugin:
  val name: String = "DFiant.plugin"
  override val description: String = "Dedicated DSL capabilities for DFiant HDL"

  def init(options: List[String]): List[PluginPhase] =
    val setting = new Setting(options.headOption)
    MetaContextDelegatePhase(setting) ::
      MetaContextGenPhase(setting) ::
      CustomIfPhase(setting) ::
      OnCreateEventsPhase(setting) ::
      Nil
