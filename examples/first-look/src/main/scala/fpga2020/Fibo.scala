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
package fpga2020
import DFiant._
import DFiant.sim.DFSimulator

trait Fibo extends DFDesign {
  val o = DFUInt[32] <> OUT
  val f = DFUInt[32] init (1, 0)
  f := f.prev + f.prev(2)
  o := f.prev(2)
}

trait FibTest extends DFSimulator {
  val fibGen = new Fibo {}
  sim.report(msg"fib: ${fibGen.o}")
}

//object FiboApp extends DFApp.VHDLCompiler[Fibo]