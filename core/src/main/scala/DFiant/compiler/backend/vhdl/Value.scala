package DFiant
package compiler.backend.vhdl

import internals._
import compiler.printer.formatter._

private object Value {
  def const(token : DFAny.Token)(implicit printer : Printer) : String = {
    import printer.config._
    token match {
      case t @ DFBits.Token(valueBits, _) if !t.isBubble => revision match {
        case _ if t.width % 4 == 0 => s"""x"${valueBits.toHex}""""
        case Revision.V2008 if t.width > 3 => s"""${t.width}x"${valueBits.toHexProper}""""
        case _ => s""""${valueBits.toBin}""""
      }
      case t @ DFBits.Token(_, _) => //bits token with bubbles
        //a hex representation may not be possible if the don't-cares are not in a complete nibble
        lazy val hexRepOption : Option[String] = t.toHexString('-', allowBinMode = false)
        lazy val binRep = t.toBinString('-')
        lazy val vhdlBin = s""""$binRep""""
        revision match {
          case Revision.V2008 if t.width > 3 => hexRepOption match {
            case Some(value) if t.width % 4 == 0 || value.head != '-' => s"""${t.width}x"$value""""
            case _ => vhdlBin
          }
          case _ => vhdlBin
        }
      case DFUInt.Token(width, Some(value)) => revision match {
        case Revision.V93 if value.bitsWidth < 31 => s"$FN to_unsigned($value, $width)"
        case Revision.V93 if width % 4 == 0 => s"""$TP unsigned'(x"${value.toBitVector(width).toHex}")"""
        case Revision.V93 => s"""$TP unsigned'("${value.toBitVector(width).toBin}")"""
        case Revision.V2008 => s"""${width}d"$value""""
      }
      case DFSInt.Token(width, Some(value)) => revision match {
        case Revision.V93 if value.bitsWidth < 31 => s"$FN to_signed($value, $width)"
        case Revision.V93 if width % 4 == 0 => s"""$TP signed'(x"${value.toBitVector(width).toHex}")"""
        case Revision.V93 => s"""$TP signed'("${value.toBitVector(width).toBin}")"""
        case Revision.V2008 if value >= 0 => s"""${width}d"$value""""
        case Revision.V2008 if value < 0 => s"""-${width}d"${-value}""""
      }
      case DFBool.Token(false, Some(value)) => if (value) "'1'" else "'0'"
      case DFBool.Token(true, Some(value)) => value.toString
      case DFEnum.Token(_, Some(entry)) => EnumTypeDcl.enumEntryFullName(entry)
      case DFUInt.Token(_, None) => const(token.bits)
      case DFSInt.Token(_, None) => const(token.bits)
      case DFEnum.Token(enumType, None) => EnumTypeDcl.enumEntryFullName(enumType.entries.head._2)
      case DFBool.Token(false, None) => "'-'"
      case DFBool.Token(true, None) => s"$LIT false"
      case t =>
        println(t)
        ???
    }
  }
  def func2(member : DFAny.Func2)(implicit printer : Printer) : String = {
    import printer.config._
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
      case Op.++ => "&"
      case _ => ???
    }
    val leftArgStr = leftArg match {
      case DFAny.Const(_,DFUInt.Token(_,Some(value)),_,_) => s"$LIT$value"
      case DFAny.Const(_,DFSInt.Token(_,Some(value)),_,_) => s"$LIT$value"
      case _ => ref(leftArg)
    }
    val rightArgStr = (member.op, rightArg) match {
      case (Op.<< | Op.>>, ra) => s"$FN to_integer(${ref(ra)})"
      case (_, DFAny.Const(_,DFUInt.Token(_,Some(value)),_,_)) => s"$LIT$value"
      case (_, DFAny.Const(_,DFSInt.Token(_,Some(value)),_,_)) => s"$LIT$value"
      case (_, ra) => ref(ra)
    }
    val dontCareComparison = (leftArg, member.op, rightArg) match {
      case (DFAny.Const(_, token, _,_), Op.== | Op.!=, _) => token.isBubble
      case (_, Op.== | Op.!=, DFAny.Const(_, token, _,_)) => token.isBubble
      case _ => false
    }
    if (dontCareComparison) {
      val comparison = revision match {
        case Revision.V93 => s"$FN std_match($leftArgStr, $rightArgStr)"
        case Revision.V2008 => s"${leftArgStr.applyBrackets()} $OP?= ${rightArgStr.applyBrackets()}"
      }
      member.op match {
        case Op.== => comparison
        case _ => s"$OP not ${comparison.applyBrackets()}"
      }
    } else (leftArg, member.op, revision) match {
      case (DFBits(_), Op.<<, Revision.V93) => s"$FN to_slv($FN shift_left($TP unsigned($leftArgStr), $rightArgStr))"
      case (DFBits(_), Op.>>, Revision.V93) => s"$FN to_slv($FN shift_right($TP unsigned($leftArgStr), $rightArgStr))"
      case (DFUInt(_) | DFSInt(_), Op.<<, Revision.V93) => s"$FN shift_left($leftArgStr, $rightArgStr)"
      case (DFUInt(_) | DFSInt(_), Op.>>, Revision.V93) => s"$FN shift_right($leftArgStr, $rightArgStr)"
      case _ => s"${leftArgStr.applyBrackets()} $OP$opStr ${rightArgStr.applyBrackets()}"
    }
  }
  def alias(member : DFAny.Alias)(implicit printer : Printer) : String = {
    import printer.config._
    val relVal = member.relValRef.get
    val relValStr = ref(relVal)
    member match {
      case toVal : DFAny.Alias.AsIs =>
        (toVal, relVal) match {
          case (l, r) if (l.dfType == r.dfType) => relValStr
          case (_, DFEnum(enumType)) => enumType match {
              case _ : EnumType.Auto[_] =>
                val enumAsIntegerStr = s"${EnumTypeDcl.enumTypeName(enumType)}$OP'pos($relValStr)"
                val enumAsUnsignedStr = s"$FN to_unsigned($enumAsIntegerStr, ${toVal.width})"
                toVal match {
                  case DFUInt(_) => enumAsUnsignedStr
                  case DFSInt(_) => s"$FN to_signed($enumAsIntegerStr, ${toVal.width})"
                  case DFBits(_) => s"$FN to_slv($enumAsUnsignedStr)"
                }
              case _ : EnumType.Manual[_] => relValStr
            }
          case (DFBits(_), _) => s"$FN to_slv($relValStr)"
          case (DFUInt(_), _) => s"$TP unsigned($relValStr)"
          case (DFSInt(_), _) => s"$TP signed($relValStr)"
          case (DFEnum(_), _) => ???
          case (DFBool(), DFBit()) => s"$FN to_bool($relValStr)"
          case (DFBit(), DFBits(w)) if (w == 1) => s"${relValStr.applyBrackets()}($LIT 0)"
          case (DFBit(), DFBool()) => s"$FN to_sl($relValStr)"
        }
      case DFAny.Alias.BitsWL(dfType, _, _, relWidth, relBitLow, _, _) =>
        val relBitHigh = relBitLow + relWidth - 1
        val bitsConv = relVal match {
          case DFBits(_) => relValStr
          case _ => s"$FN to_slv($relValStr)"
        }
        dfType match {
          case DFBool.Type(false) => s"${bitsConv.applyBrackets()}($LIT$relBitLow)"
          case DFBits.Type(_) =>
            if (relVal.width == relWidth) bitsConv
            else s"${bitsConv.applyBrackets()}($LIT$relBitHigh $KW downto $LIT$relBitLow)"
        }
      case DFAny.Alias.Resize(dfType, _, _, _) => s"$FN resize($relValStr, $LIT${dfType.width})"
      case _ : DFAny.Alias.Invert => s"$OP not ${relValStr.applyBrackets()}"
      case _ : DFAny.Alias.Prev => ??? //should not happen since prev is removed via clocking phase
    }
  }

  def ref(member : DFAny.Member)(implicit printer : Printer) : String = {
    import printer.config._
    member match {
      case c : DFAny.Const => const(c.token)
      case d : DFAny.Dcl => d.getOwnerBlock match {
        case DFDesign.Block.Internal(_,_,_,Some(rep)) => rep match {
          case EdgeDetect.Rep(bitRef, EdgeDetect.Edge.Rising) => s"$OP rising_edge(${ref(bitRef)})"
          case EdgeDetect.Rep(bitRef, EdgeDetect.Edge.Falling) => s"$OP falling_edge(${ref(bitRef)})"
          case _ => ??? //missing support for other inlined options
        }
        case _ => d.name
      }
      case m if m.isAnonymous => Value(m)
      case m => m.name
    }
  }
  def apply(member : DFAny.Member)(implicit printer : Printer) : String = member match {
    case c : DFAny.Const => const(c.token)
    case f : DFAny.Func2 => func2(f)
    case a : DFAny.Alias => alias(a)
    case _ : DFAny.Dcl => ??? //shouldn't occur
    case _ : DFAny.Dynamic => ??? //shouldn't occur
    case _ : DFAny.Fork => ??? //shouldn't occur
  }
}

