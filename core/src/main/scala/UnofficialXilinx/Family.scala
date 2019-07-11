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

package UnofficialXilinx

sealed trait Family {

}
object Family {
  trait Artix7 extends Family with Series.`7`
  trait Artix7LowVoltage extends Family with Series.`7`
  trait Kintex7 extends Family with Series.`7`
  trait Virtex7 extends Family with Series.`7`
  trait Kintex7LowVoltage extends Family with Series.`7`
  trait KintexUltraScale extends Family with Series.`7`
  trait XAArtix7 extends Family with Series.`7`
  trait XAZync7000 extends Family with Series.`7`
  trait Zync7000 extends Family with Series.`7`
}
