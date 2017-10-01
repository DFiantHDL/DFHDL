package DFiant.core

import singleton.ops._
import singleton.twoface._


//trait DFUFix[IWL, FWL] extends DFAny.Val[IWL, DFUFix[IWL, FWL], DFUFix.Var[IWL, FWL]] {
//  val iwl : TwoFace.Int[IWL]
//  val fwl : TwoFace.Int[FWL]
//  //    type Width = wl.Out
//  //    val fw = protBits(fwl-1, 0)
//  def newEmptyDFVar: TVar = DFUFix.create(iwl, fwl)
//  def dfTypeName: String = "DFUFix"
//}
//
//object DFUFix {
//  trait Var[IWL, FWL] extends DFUFix[IWL, FWL] with DFAny.Var[1, DFUFix[IWL, FWL], DFUFix.Var[IWL, FWL]]
//  def create[IWL, FWL](_iwl : TwoFace.Int[IWL], _fwl : TwoFace.Int[FWL])(implicit tfc : TwoFace.Int.Shell2[+, IWL, Int, FWL, Int]) : Var[IWL, FWL] = new Var[IWL, FWL] {
//    val iwl = _iwl
//    val fwl = _fwl
//    val width = TwoFace.Int.create[1](1)
//  }
//  def apply[IWL, FWL](_iwl : TwoFace.Int[IWL], _fwl : TwoFace.Int[FWL]) : Var[IWL, FWL] = create(_iwl, _fwl)
//}
