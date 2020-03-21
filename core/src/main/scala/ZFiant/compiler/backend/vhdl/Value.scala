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
    import printer.config._
    import formatter._
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
      case Op.~~ => "&"
      case _ => ???
    }
    val leftArgStr = ref(leftArg)
    val rightArgStr = (member.op, rightArg) match {
      case (Op.<< | Op.>>, ra : DFAny.Const) => ra.token match {
        case DFUInt.Token(_,value,false) => s"$LIT$value"
      }
      case (Op.<< | Op.>>, ra) => s"$FN to_integer(${ref(ra)})"
      case (_, ra) => ref(ra)
    }
    s"${leftArgStr.applyBrackets()} $OP$opStr ${rightArgStr.applyBrackets()}"
  }
  def alias(member : DFAny.Alias[_ <: DFAny.Type,_ <: DFAny,_ <: DFAny.Modifier])(implicit printer : Printer) : String = {
    import printer.config._
    import formatter._
    val relVal = member.relValRef.get
    val relValStr = ref(relVal)
    member match {
      case toVal : DFAny.Alias.AsIs => (toVal, relVal) match {
        case (l, r) if (l.dfType == r.dfType) => relValStr
        case (DFBits(_), _) => s"$FN to_slv($relValStr)"
        case (DFUInt(_), _) => s"$TP unsigned($relValStr)"
        case (DFSInt(_), _) => s"$TP signed($relValStr)"
        case (DFEnum(_), _) => ???
        case (DFBool(), DFBit()) => s"$FN to_bool($relValStr)"
        case (DFBit(), DFBits(w)) if (w.getValue == 1) => s"${relValStr.applyBrackets()}($LIT 0)"
        case (DFBit(), DFBool()) => s"$FN to_sl($relValStr)"
      }
      case DFAny.Alias.BitsWL(_, _, _, relWidth, relBitLow, _, _) =>
        val relBitHigh = relBitLow + relWidth - 1
        val bitsConv = relVal match {
          case DFBits(_) => relValStr
          case _ => s"$FN to_slv($relValStr)"
        }
        if (relVal.width.getValue == relWidth) bitsConv
        else s"${bitsConv.applyBrackets()}($LIT$relBitHigh $KW downto $LIT$relBitLow)"
      case DFAny.Alias.Resize(dfType, _, _, _) => s"$FN resize($relValStr, $LIT${dfType.width})"
      case _ : DFAny.Alias.Invert => s"$OP not ${relValStr.applyBrackets()}"
      case _ : DFAny.Alias.Prev => ??? //should not happen since prev is removed via clocking phase
    }
  }

  def ref(member : DFAny)(implicit printer : Printer) : String = {
    import printer.config._
    member match {
      case c : DFAny.Const => const(c.token)
      case d : DFAny.Dcl => d.ownerRef.get match {
        case DFDesign.Block.Internal(_,_,_,Some(rep)) => rep match {
          case Rising.Rep(bitRef) => s"$OP rising_edge(${ref(bitRef)})"
          case _ => ??? //missing support for other inlined options
        }
        case _ => d.name
      }
      case m if m.isAnonymous => Value(m)
      case m => m.name
    }
  }
  def apply(member : DFAny)(implicit printer : Printer) : String = member match {
    case c : DFAny.Const => const(c.token)
    case f : DFAny.Func2 => func2(f).toString
    case a : DFAny.Alias[_,_,_] => alias(a)
    case _ : DFAny.Dcl => ??? //shouldn't occur
  }
}

