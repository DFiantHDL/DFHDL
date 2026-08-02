package dfhdl.plugin

import scala.language.implicitConversions

import dotty.tools.dotc._
import core._
import Contexts._
import Symbols._
import Flags._
import SymDenotations._

import Decorators._
import ast.Trees._
import ast.tpd

/** The `-P:dfhdl.plugin:<option>` values this compilation passed to the plugin.
  *
  * Every option the plugin knows is a bare flag, and the first value that is not one is taken as
  * the config file path. A new flag therefore has to be registered in [[Setting.flagNames]], or it
  * would silently be read as that path instead of as a flag.
  */
class Setting(options: List[String]):
  import Setting.*

  val configFile: Option[String] = options.filterNot(flagNames).headOption

  /** Enables the `PluginErrCheck` interceptor phase, which exists only for DFHDL's own test
    * compilations (see devdocs/plugin-error-testing.md).
    */
  val testing: Boolean = options.contains(Testing)

  /** Leaves the DFHDL type printer and the diagnostic re-reporter uninstalled, so the compiler
    * reports types in its own vocabulary (see [[DFHDLTypePrinter]] and `PreTyperPhase`).
    *
    * Opt-in, and meant for working on the DSL itself: when a diagnostic is hard to read, this is
    * how to tell a genuinely confusing error apart from one the printer made confusing, without
    * editing the plugin to find out.
    */
  val disableCustomPrinter: Boolean = options.contains(DisableCustomPrinter)
end Setting

object Setting:
  final val Testing = "testing"
  final val DisableCustomPrinter = "disableCustomPrinter"
  private val flagNames: Set[String] = Set(Testing, DisableCustomPrinter)
end Setting
