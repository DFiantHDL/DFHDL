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
    val dfVals = tuple.toList.map { case dfVal: DFValAny => dfVal }
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
  def patternSingletonInt(selector: DFValAny, value: Int): Pattern =
    val token = DFDecimal.Token(
      selector.dfType.asIR.asFE[DFUInt[Int]],
      Some(BigInt(value))
    )
    Pattern.Singleton(token.asIR)
  def patternSingletonEnum(selector: DFValAny, entry: DFEncoding): Pattern =
    Pattern.Singleton(
      ir.DFToken.forced(selector.dfType.asIR, Some(entry.value))
    )
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
