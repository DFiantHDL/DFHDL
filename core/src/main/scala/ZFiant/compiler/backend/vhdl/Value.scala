package ZFiant
package compiler.backend.vhdl

private object Value {
  def const(token : DFAny.Token)(implicit printer : Printer) : String = token match {
    case DFBits.Token(width, value, _) => if (width % 4 == 0) s"""x"${value.toHex}"""" else s""""${value.toBin}""""
    case DFUInt.Token(width, value, _) => s"""$width"$value""""
    case DFSInt.Token(width, value, _) => s"""$width"$value""""
    case DFBool.Token(false, value, _) => if (value) "'1'" else "'0'"
    case DFBool.Token(true, value, _) => value.toString
    case DFEnum.Token(enumType, value) => s"${enumType.name}_${value.get.name}"
    case _ => ???
  }
  def func2(member : DFAny.Func2)(implicit printer : Printer) : String = {
    import printer.config.formatter._
    val leftArg = member.leftArgRef.get
    val rightArg = member.rightArgRef.get
    import DFAny.Func2.Op
    val opStr = member.op match {
      case Op.+ => "+"
      case Op.- => "-"
      case Op.* => "*"
      case Op.== => "="
      case Op.!= => "/="
      case Op.< => "<"
      case Op.> => ">"
      case Op.<= => "<="
      case Op.>= => "=>"
      case Op.| | Op.|| => "or"
      case Op.& | Op.&& => "and"
      case Op.^ => "xor"
      case Op.<< => leftArg match {
        case DFSInt(_) => "sla"
        case _ => "sll"
      }
      case Op.>> => leftArg match {
        case DFSInt(_) => "sra"
        case _ => "srl"
      }
      case _ => ???
    }
    val leftArgStr = ref(leftArg)
    val rightArgStr = (member.op, rightArg) match {
      case (Op.<< | Op.>>, ra : DFAny.Const) => ra.token match {
        case DFUInt.Token(_,value,false) => value.toString()
      }
      case (Op.<< | Op.>>, ra) => s"to_integer(${ref(ra)})"
      case (_, ra) => ref(ra)
    }
    s"${leftArgStr.applyBrackets()} $opStr ${rightArgStr.applyBrackets()}"
  }
  def alias(member : DFAny.Alias[_ <: DFAny.Type,_ <: DFAny,_ <: DFAny.Modifier])(implicit printer : Printer) : String = {
    import printer.config._
    import formatter._
    val relVal = member.relValRef.get
    val relValStr = ref(relVal)
    member match {
      case toVal : DFAny.Alias.AsIs => (toVal, relVal) match {
        case (l, r) if (l.dfType == r.dfType) => relValStr
        case (DFBits(_), _) => s"std_logic_vector($relValStr)"
        case (DFUInt(_), _) => s"unsigned($relValStr)"
        case (DFSInt(_), _) => s"signed($relValStr)"
        case (DFEnum(_), _) => ???
        case (DFBit(), DFBits(w)) if (w.getValue == 1) => s"${relValStr.applyBrackets()}(0)"
      }
      case DFAny.Alias.BitsWL(_, _, _, relWidth, relBitLow, _, _) =>
        val relBitHigh = relBitLow + relWidth - 1
        s"${relValStr.applyBrackets()}($relBitHigh, $relBitLow)"
      case DFAny.Alias.Resize(dfType, _, _, _) => s"resize($relValStr, ${dfType.width})"
      case _ : DFAny.Alias.Invert => s"not ${relValStr.applyBrackets()}"
      case _ : DFAny.Alias.Prev => ??? //should not happen since prev is removed via clocking phase
    }
  }

  def ref(member : DFAny)(implicit printer : Printer) : String = member match {
    case c : DFAny.Const => const(c.token)
//    case Rising(bit) => s"rising_edge(${ref(bit)})"
    case m if m.isAnonymous => Value(m)
    case m => m.name
  }
  def apply(member : DFAny)(implicit printer : Printer) : String = member match {
    case c : DFAny.Const => const(c.token)
    case f : DFAny.Func2 => func2(f).toString
    case a : DFAny.Alias[_,_,_] => alias(a)
    case _ : DFAny.Dcl => ???
  }
  def foo(implicit printer : Printer) : String = {
    implicitly[MemberGetSet]
    ""
  }
}

