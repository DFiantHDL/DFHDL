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

package Xilinx

trait Device {
  trait Resources {
    val Slices : Int with Singleton
    val LogicCells : Int with Singleton
    val FlipFlops : Int with Singleton
    val BlockRAMs36Kb : Int with Singleton
    val CMTs : Int with Singleton
    val DSPs : Int with Singleton
    val GTPE2Trans : Int with Singleton
    val GbTrans : Int with Singleton
    val IOBs : Int with Singleton
  }
//  lazy val resources : Resources = ???

}

object Device {
  trait XC7K70T extends Device with Family.Kintex7 {
  }
  trait XC7K160T extends Device with Family.Kintex7 {
  }
  trait XC7K325T extends Device with Family.Kintex7 {
  }
  trait XC7K355T extends Device with Family.Kintex7 {
  }
  trait XC7K410T extends Device with Family.Kintex7 {
  }
  trait XC7K420T extends Device with Family.Kintex7 {
  }
  trait XC7K480T extends Device with Family.Kintex7 {
  }

  trait XC7V585T extends Device with Family.Virtex7 {
  }
  trait XC7V2000T extends Device with Family.Virtex7 {
  }
  trait XC7VX330T extends Device with Family.Virtex7 {
  }
  trait XC7VX415T extends Device with Family.Virtex7 {
  }
  trait XC7VX485T extends Device with Family.Virtex7 {
  }
  trait XC7VX550T extends Device with Family.Virtex7 {
  }
  trait XC7VX690T extends Device with Family.Virtex7 {
  }
  trait XC7VX980T extends Device with Family.Virtex7 {
  }
  trait XC7VX1140T extends Device with Family.Virtex7 {
  }
  trait XC7VH580T extends Device with Family.Virtex7 {
  }
  trait XC7VH870T extends Device with Family.Virtex7 {
  }
}
