//package DFiant.fixedpoint
//
//import DFiant.core._
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
//  def dfTypeName: String = "DFUFix"
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
