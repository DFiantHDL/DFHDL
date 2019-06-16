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

//package DFiant.fixedpoint
//
//import DFiant._
//import singleton.ops._
//import singleton.twoface._
//
//
////DFUFix can be derived from DFStruct, however to increase (Scala) compile-time safety, we rather
////define local field aliasing manually
//trait DFUFix[IWL, FWL] extends DFAny.Val[IWL + FWL, DFUFix[IWL, FWL], DFUFix.Var[IWL, FWL]] {
//  val iwl : TwoFace.Int[IWL]
//  val fwl : TwoFace.Int[FWL]
//
//  val iw : TBits[IWL]
//  val fw : TBits[FWL]
//  def newEmptyDFVar: TVar = DFUFix.create(iwl, fwl)
//}
//
//object DFUFix {
//  trait Var[IWL, FWL] extends DFUFix[IWL, FWL] with DFAny.Var[IWL + FWL, DFUFix[IWL, FWL], DFUFix.Var[IWL, FWL]] {
//
//  }
//  def create[IWL, FWL](_iwl : TwoFace.Int[IWL], _fwl : TwoFace.Int[FWL]) : Var[IWL, FWL] = {
//    val width = _iwl + _fwl
//    new DFAny.NewVar[width.Out](width) with Var[IWL, FWL] {
//      val iwl = _iwl
//      val fwl = _fwl
//      val iw : TBits[IWL] = protBits(_fwl+_iwl-1, _fwl).asInstanceOf[TBits[IWL]]
//      val fw : TBits[FWL] = protBits(_fwl-1, 0).asInstanceOf[TBits[FWL]]
//    }
//  }
//
//  def apply[IWL, FWL](_iwl : TwoFace.Int[IWL], _fwl : TwoFace.Int[FWL]) = create(_iwl, _fwl)
//  implicit def apply[IWL, FWL](implicit _iwl : TwoFace.Int[IWL], _fwl : TwoFace.Int[FWL], di: DummyImplicit) = create(_iwl, _fwl)
//}
//
