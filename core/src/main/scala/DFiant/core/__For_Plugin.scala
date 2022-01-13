package DFiant.core
import DFiant.compiler.ir
import DFVal.Func.Op as FuncOp
import DFiant.compiler.ir.DFVal.Modifier
import ir.DFConditional.DFCaseBlock.Pattern

import collection.immutable.ListMap
object __For_Plugin:
  def toFunc1[R](block: => R): () => R = () => block
  def toTuple2[T1, T2](t1: T1, t2: T2): (T1, T2) = (t1, t2)
  def toTuple3[T1, T2, T3](t1: T1, t2: T2, t3: T3): (T1, T2, T3) = (t1, t2, t3)
  def fromBoolean(value: Boolean)(using DFC): DFValOf[DFBool] =
    DFVal.Const(DFBoolOrBit.Token(DFBool, value))
  // tuple of DFVals "concatenated" to be a DFVal of type tuple
  def structToDFVal[V <: DFValAny](product: FieldsOrTuple)(using DFC): V =
    DFVal.OrTupleOrStruct.unapply(product).get.asInstanceOf[V]
  def structDFValSelect[V <: DFValAny](dfVal: DFValAny, fieldName: String)(using
      DFC
  ): V =
    DFVal.Alias
      .SelectField(dfVal, fieldName)(using dfc.anonymize)
      .asInstanceOf[V]
  def patternSingleton(selector: DFValAny, value: Any): Pattern =
    val tokenIR = (selector.dfType.asIR, value) match
      case (dt: ir.DFBoolOrBit, v: Int) if v == 0 | v == 1 =>
        ir.DFBoolOrBit.Token(dt, Some(v > 0))
      case (dt: ir.DFBoolOrBit, v: Boolean) =>
        ir.DFBoolOrBit.Token(dt, Some(v))
      case (dt: ir.DFBits, allBit: BitOrBool) =>
        DFBits.Token(dt.width, SameElementsVector(allBit)).asIR
      case (dt: ir.DFDecimal, v: Int) =>
        ir.DFDecimal.Token(dt, Some(BigInt(v)))
      case (dt: ir.DFEnum, v: DFEncoding) =>
        ir.DFEnum.Token(dt, Some(v.bigIntValue))
      case (dt: ir.DFStruct, v) => ???
      case _                    => ???
    Pattern.Singleton(tokenIR)
  end patternSingleton
  def patternSingletonSI(si: Any): Pattern =
    si match
      case Some(Seq(DFVal(const: ir.DFVal.Const))) =>
        Pattern.Singleton(const.token)
      case _ => println(si); ???
  def patternAlternative(list: List[Pattern]): Pattern =
    Pattern.Alternative(list)
  def patternStruct(name: String, list: List[Pattern]): Pattern =
    Pattern.Struct(name, list)
  def patternCatchAll: Pattern = Pattern.CatchAll
  def extractValDcl[V <: DFValAny](selector: V, extractName: String)(using
      DFC
  ): V =
    val dcl =
      DFVal.Dcl(selector.dfType, Modifier.VAR)(using dfc.setName(extractName))
    dcl.assign(DFVal.Const(Bubble(selector.dfType)))
    dcl.asInstanceOf[V]
  def forcedAssign(toVal: DFValAny, fromVal: DFValAny)(using DFC): Unit =
    toVal.asInstanceOf[DFVarOf[DFTypeAny]].assign(fromVal)
  def bindVal[V <: DFValAny](selector: V, bindName: String)(using DFC): V =
    DFVal.Alias.AsIs
      .ident(selector)(using dfc.setName(bindName))
      .tag(Pattern.Bind.Tag)
      .asInstanceOf[V]
  def bindValRange[V <: DFValAny](
      selector: V,
      bindName: String,
      relBitHigh: Int,
      relBitLow: Int
  )(using DFC): V =
    val dfType = selector.dfType.asIR
    val selectorBitsIR: ir.DFVal = dfType match
      case _: ir.DFBits => selector.asIR
      case _ =>
        import DFVal.Ops.bits
        selector
          .bits(using Width.wide)(using dfc.anonymize)
          .asIR
    DFVal.Alias
      .ApplyRange(selectorBitsIR.asValOf[DFBits[Int]], relBitHigh, relBitLow)(
        using dfc.setName(bindName)
      )
      .tag(Pattern.Bind.Tag)
      .asInstanceOf[V]
  end bindValRange
  def patternBind(bindVal: DFValAny, pattern: Pattern)(using DFC): Pattern =
    Pattern.Bind(bindVal.asIR.ref, pattern)
  def patternBindSI(op: String, parts: List[String], bindVals: List[DFValAny])(using
      DFC
  ): Pattern =
    Pattern.BindSI(op, parts, bindVals.map(_.asIR.ref))

end __For_Plugin
