//package DFiant.core
//
//trait DFUnion[Val <: DFAny, Var <: Val with DFAny.Var[WUnsafe, Val, Var]] extends DFAny.Val[WUnsafe, Val, Var] {
//  this : Val =>
//  type TField1 <: DFAny
//  type TField2 <: DFAny
//  type TField3 <: DFAny
//  type TField4 <: DFAny
//  type TField5 <: DFAny
//  private var privWidth : Int = 0
//  val width = privWidth
//
//  type TAlias1 <: TField1#TVal
//  type TAlias2 <: TField2#TVal
//  type TAlias3 <: TField3#TVal
//  type TAlias4 <: TField4#TVal
//  type TAlias5 <: TField5#TVal
//
//  protected[DFiant] def privInsert(dfVar : DFAny)
//
//  protected def insert(dfVar : DFBits[WUnsafe]#TVar) : TBits[WUnsafe] = {
//    privInsert(dfVar)
//    dfVar.asInstanceOf[TBits[WUnsafe]]
//  }
//  protected def insert(dfVar : DFBool#TVar) : TBool = {
//    privInsert(dfVar)
//    dfVar.asInstanceOf[TBool]
//  }
////  protected def insert(dfVar : DFUInt#TVar) : TUInt = {
////    privInsert(dfVar)
////    dfVar.asInstanceOf[TUInt]
////  }
//
//  protected def insert(dfVar : TField1#TVar) : TAlias1 = {
//    privInsert(dfVar)
//    dfVar.asInstanceOf[TAlias1]
//  }
//  protected def insert(dfVar : TField2#TVar)(implicit d1: DummyImplicit) : TAlias2 = {
//    privInsert(dfVar)
//    dfVar.asInstanceOf[TAlias2]
//  }
//  protected def insert(dfVar : TField3#TVar)(implicit d1: DummyImplicit, d2: DummyImplicit) : TAlias3 = {
//    privInsert(dfVar)
//    dfVar.asInstanceOf[TAlias3]
//  }
//  protected def insert(dfVar : TField4#TVar)(implicit d1: DummyImplicit, d2: DummyImplicit, d3: DummyImplicit) : TAlias4 = {
//    privInsert(dfVar)
//    dfVar.asInstanceOf[TAlias4]
//  }
//  protected def insert(dfVar : TField5#TVar)(implicit d1: DummyImplicit, d2: DummyImplicit, d3: DummyImplicit, d4: DummyImplicit) : TAlias5 = {
//    privInsert(dfVar)
//    dfVar.asInstanceOf[TAlias5]
//  }
//}
//
//abstract class DFUnionVarA[Val <: DFAny, Var <: Val with DFAny.Var[WUnsafe, Val, Var]] extends DFAny.Var[WUnsafe, Val, Var] with DFUnion[Val, Var] {
//  this : Val with Var =>
//  type TAlias1 = TField1#TVar
//  type TAlias2 = TField2#TVar
//  type TAlias3 = TField3#TVar
//  type TAlias4 = TField4#TVar
//  type TAlias5 = TField5#TVar
//
//  protected[DFiant] def privInsert(dfVar : DFAny) = {
//    ???
//  }
//}
