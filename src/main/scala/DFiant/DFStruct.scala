//package DFiant
//
//import DFiant.internals._
//trait DFStruct[Val <: DFAny] extends DFAny.Val[WUnsafe, Val, Val with DFStruct.Var[Val]] {
//  this : Val =>
//  type TField1 <: DFAny
//  type TField2 <: DFAny
//  type TField3 <: DFAny
//  type TField4 <: DFAny
//  type TField5 <: DFAny
//  private var privPosLSB : Int = 0
//
//  type TAlias1 <: TField1#TVal
//  type TAlias2 <: TField2#TVal
//  type TAlias3 <: TField3#TVal
//  type TAlias4 <: TField4#TVal
//  type TAlias5 <: TField5#TVal
//
//  protected[DFiant] def privInsert(dfVar : DFAny) = {
//    almanacEntry.structEntryList += dfVar.almanacEntry
//    privPosLSB = privPosLSB + dfVar.width
//  }
//
//  protected def insert[WVar](dfVar : DFBits[WVar]#TVar) : TBits[WVar] = {
//    privInsert(dfVar)
//    dfVar.asInstanceOf[TBits[WVar]]
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
//
//  override protected[DFiant] lazy val almanacEntry : AlmanacEntryStruct =  AlmanacEntryStruct(width)
//
//}
//
//
//object DFStruct {
//  abstract class Var[Val <: DFAny]() extends DFAny.Var[WUnsafe, Val, Val with DFStruct.Var[Val]] with DFStruct[Val] {
//    this : Val =>
//    type TAlias1 = TField1#TVar
//    type TAlias2 = TField2#TVar
//    type TAlias3 = TField3#TVar
//    type TAlias4 = TField4#TVar
//    type TAlias5 = TField5#TVar
//  }
//
//  import scala.reflect.macros.blackbox.Context
//  import scala.language.experimental.macros
//
//  def helper[Val <: DFAny : c.WeakTypeTag, PVar : c.WeakTypeTag](c : Context)() : c.Expr[Val] = {
//    import c.universe._
//    val weakVal = weakTypeOf[Val]
//    val sym = symbolOf[PVar]
//    val valTree = tq"$weakVal"
//    val appliedTree = tq"$sym[$weakVal]"
//    val list = List(appliedTree, valTree)
//    val className = c.freshName()
//    val classType = TypeName(className)
//    val classTerm = TermName(className)
//    val genTree = q"""
//        case class $classType() extends ..$list {
//          def newEmptyDFVar = copy().asInstanceOf[TVar]
//        }
//        $classTerm()
//      """
//    c.Expr(genTree)
//  }
//  def apply[Val <: DFAny]() : DFStruct.Var[Val] with Val = macro helper[Val, DFStruct.Var[Val]] //GenVarWithVal[Val, DFStruct.Var[Val]].asInstanceOf[DFStruct.Var[Val] with Val]//
//}
//
