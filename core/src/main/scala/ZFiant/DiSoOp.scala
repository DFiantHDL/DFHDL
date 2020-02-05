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

package ZFiant

sealed trait DiSoOp
//Dual Input, Single Output Operation
object DiSoOp {
  sealed trait Negateable extends DiSoOp {
    def negate : Negateable
  }
  sealed trait +  extends Negateable {
    def negate : - = -
  }
  sealed trait -  extends Negateable {
    def negate : + = +
  }
  sealed trait *  extends DiSoOp
  sealed trait +^  extends Negateable {
    def negate : -^ = -^
  }
  sealed trait -^  extends Negateable {
    def negate : +^ = +^
  }
  sealed trait *^  extends DiSoOp
  sealed trait == extends DiSoOp {
    override def toString: String = "==="
  }
  sealed trait != extends DiSoOp {
    override def toString: String = "=!="
  }
  sealed trait <  extends DiSoOp
  sealed trait >  extends DiSoOp
  sealed trait <= extends DiSoOp
  sealed trait >= extends DiSoOp
  sealed trait |  extends DiSoOp
  sealed trait &  extends DiSoOp
  sealed trait ^  extends DiSoOp
  sealed trait Shift extends DiSoOp
  sealed trait << extends Shift
  sealed trait >> extends Shift
  sealed trait || extends DiSoOp
  sealed trait && extends DiSoOp
  implicit case object +  extends +
  implicit case object -  extends -
  implicit case object *  extends *
  implicit case object +^  extends +^
  implicit case object -^  extends -^
  implicit case object *^  extends *^
  implicit case object == extends ==
  implicit case object != extends !=
  implicit case object <  extends <
  implicit case object >  extends >
  implicit case object <= extends <=
  implicit case object >= extends >=
  implicit case object |  extends |
  implicit case object &  extends &
  implicit case object ^  extends ^
  implicit case object << extends <<
  implicit case object >> extends >>
  implicit case object || extends ||
  implicit case object && extends &&
}
