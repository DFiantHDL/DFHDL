package DFiant.core
import DFiant.compiler.ir
import DFVal.Func.Op as FuncOp
import ir.DFConditional.DFCaseBlock.Pattern
object __For_Plugin:
  def toFunc1[R](block: => R): () => R = () => block
  def toTuple2[T1, T2](t1: T1, t2: T2): (T1, T2) = (t1, t2)
  def toTuple3[T1, T2, T3](t1: T1, t2: T2, t3: T3): (T1, T2, T3) = (t1, t2, t3)
  def fromBoolean(value: Boolean)(using DFC): DFValOf[DFBool] =
    DFVal.Const(DFBoolOrBit.Token(DFBool, value))
  // tuple of DFVals "concatenated" to be a DFVal of type tuple
  def tupleToDFVal[V <: DFValAny](tuple: Tuple)(using DFC): V =
    val dfVals = tuple.toList.map {
      case dfVal: DFValAny => dfVal
      case internal: Tuple => tupleToDFVal(internal)
    }
    val dfType = DFTuple[NonEmptyTuple](dfVals.map(_.dfType))
    DFVal.Func(dfType, FuncOp.++, dfVals)(using dfc.anonymize).asInstanceOf[V]
  def tupleDFValSelect[V <: DFValAny](dfVal: DFValAny, index: Int)(using
      DFC
  ): V =
    import DFTuple.Val.Ops.applyForced
    dfVal.asIR
      .asValOf[DFTuple[NonEmptyTuple]]
      .applyForced(index)(using dfc.anonymize)
      .asInstanceOf[V]
  def patternSingleton(selector: DFValAny, value: Any): Pattern =
    val tokenIR = (selector.dfType.asIR, value) match
      case (dt: ir.DFBoolOrBit, v: Int) if v == 0 | v == 1 =>
        ir.DFBoolOrBit.Token(dt, Some(v > 0))
      case (dt: ir.DFBoolOrBit, v: Boolean) =>
        ir.DFBoolOrBit.Token(dt, Some(v))
      case (dt: ir.DFBits, v) => ???
      case (dt: ir.DFDecimal, v: Int) =>
        ir.DFDecimal.Token(dt, Some(BigInt(v)))
      case (dt: ir.DFEnum, v: DFEncoding) =>
        ir.DFEnum.Token(dt, Some(v.value))
      case (dt: ir.DFStruct, v) => ???
      case _                    => ???
    Pattern.Singleton(tokenIR)
  end patternSingleton
  def patternSingletonSI(si: Any): Pattern =
    si match
      case Some(Seq(DFVal(const: ir.DFVal.Const))) =>
        Pattern.Singleton(const.token)
      case _ => println(si); ???
  def patternAlternative(list: List[Any]): Pattern =
    Pattern.Alternative(list.asInstanceOf[List[Pattern]])
  def patternTuple(list: List[Pattern]): Pattern =
    Pattern.Tuple(list)
  def patternCatchAll: Pattern = Pattern.CatchAll
end __For_Plugin
