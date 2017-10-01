package DFiant.fixedpoint

import DFiant.core._
import singleton.ops._
import singleton.twoface._

abstract class DFUFix[IWL, FWL] extends DFAny.ValW[IWL + FWL, DFUFix[IWL, FWL], DFUFix.Var[IWL, FWL]] {
  val iwl : TwoFace.Int[IWL]
  val fwl : TwoFace.Int[FWL]

//  def fw(implicit tfc : TwoFace.Int.Shell2[-,FWL,Int,1,Int]) = protBits(tfc(fwl,1), 0)
  def newEmptyDFVar: TVar = DFUFix.create(iwl, fwl)
  def dfTypeName: String = "DFUFix"
}

object DFUFix {
  trait Var[IWL, FWL] extends DFUFix[IWL, FWL] with DFAny.VarW[IWL + FWL, DFUFix[IWL, FWL], DFUFix.Var[IWL, FWL]] {

  }
  def create[IWL, FWL](_iwl : TwoFace.Int[IWL], _fwl : TwoFace.Int[FWL]) : Var[IWL, FWL] = new Var[IWL, FWL] {
    val width = TwoFace.Int.create[Width](_iwl + _fwl)
    val iwl = _iwl
    val fwl = _fwl
  }

  def apply[IWL, FWL](_iwl : TwoFace.Int[IWL], _fwl : TwoFace.Int[FWL]) = create(_iwl, _fwl)
}

