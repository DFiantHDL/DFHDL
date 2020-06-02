/*
 *     This file is part of DFiant.
 *
 *     DFiant is free software: you can redistribute it and/or modify
 *     it under the terms of the Lesser GNU General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     any later version.
 *
 *     DFiant is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     Lesser GNU General Public License for more details.
 *
 *     You should have received a copy of the Lesser GNU General Public License
 *     along with DFiant.  If not, see <https://www.gnu.org/licenses/>.
 */

package DFiant
package sim
package tools
import DFiant.compiler.backend.vhdl
import shapeless.{:: => #:}
import compiler.Compilation
import compiler.backend.vhdl.VHDLBackend
package object ghdl {
  trait GHDL extends Simulation.Tool
  implicit def simulator[D <: DFSimDesign, R <: vhdl.Revision] : Simulator[D, VHDLBackend[R], GHDL] = ???
//  implicit def evGHDL[D <: DFDesign, S <: shapeless.HList, C](c : C)(
//    implicit conv : C => Compilable[D, VHDLCompiled #: S]
//  ) : GHDLOps[D, S] = new GHDLOps[D, S](conv(c))

}
