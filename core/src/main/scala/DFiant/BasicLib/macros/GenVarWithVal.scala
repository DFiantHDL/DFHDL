/*
 *     This file is part of DFiant.
 *
 *     DFiant is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU Lesser General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     any later version.
 *
 *     DFiant is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     GNU Lesser General Public License for more details.
 *
 *     You should have received a copy of the GNU Lesser General Public License
 *     along with DFiant.  If not, see <https://www.gnu.org/licenses/>.
 */

package DFiant.BasicLib.macros

import DFiant.DFAny

object GenVarWithVal {
  import scala.reflect.macros.blackbox.Context
  import scala.language.experimental.macros

  def macroImpl[Val <: DFAny : c.WeakTypeTag, Var : c.WeakTypeTag](c : Context)() : c.Expr[Val] = {
    import c.universe._
    val weakVal = weakTypeOf[Val]
    val sym = symbolOf[Var]
    val valTree = tq"$weakVal"
    val appliedTree = tq"$sym[$weakVal]"
    val list = List(appliedTree, valTree)
    val className = c.freshName()
    val classType = TypeName(className)
    val classTerm = TermName(className)
    val genTree = q"""
        case class $classType() extends ..$list {
          def newEmptyDFVar = copy().asInstanceOf[TVar]
        }
        $classTerm()
      """
    c.Expr(genTree)
  }
  def apply[Val <: DFAny, Var]() : Val = macro macroImpl[Val, Var]

}
