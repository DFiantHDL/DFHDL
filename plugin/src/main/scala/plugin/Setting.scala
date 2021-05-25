package DFiant.plugin

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

class Setting(configFile: Option[String]) {}
