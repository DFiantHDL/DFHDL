package DFiant
package compiler.backend.verilog
import DFAny.Func2.Op
import compiler.printer.formatter._

private object Value {
  def const(token : DFAny.Token)(implicit printer : Printer) : String = {
    import printer.config._
    token match {
      case t @ DFBits.Token(_, _) =>
        val hexRepOption : Option[String] = t.toHexString('?', allowBinMode = false)
        hexRepOption match {
          case Some(value) if t.width > 3 => s"""${t.width}'h$value"""
          case _ => s"""${t.width}'b${t.toBinString('?')}"""
        }
      case t if t.isBubble => const(t.bits)
      case DFUInt.Token(width, Some(value)) => s"""${width}'d${value}"""
      case DFSInt.Token(width, Some(value)) =>
        if (value >= 0) s"""${width}'sd${value}"""
        else s"""-${width}'sd${-value}"""
      case DFBool.Token(true, Some(value)) => if (value) "1" else "0"
      case DFBool.Token(false, Some(value)) => if (value) "1'b1" else "1'b0"
      case DFEnum.Token(_, entry) => EnumTypeDcl.enumEntryRefName(entry.get)
      case t =>
        println(t)
        ???
    }
  }
  def func2(member : DFAny.Func2)(implicit printer : Printer) : String = {
    import printer.config._
    val leftArg = member.leftArgRef.get
    val rightArg = member.rightArgRef.get
    val opStr = member.op match {
      case Op.<< => leftArg match {
        case DFSInt(_) => "<<<"
        case _ => "<<"
      }
      case Op.>> => leftArg match {
        case DFSInt(_) => ">>>"
        case _ => ">>"
      }
      case Op.== => "=="
      case Op.!= => "!="
      case op => op.toString
    }
    val leftArgStr = leftArg match {
      case DFAny.Const(_,DFUInt.Token(_,Some(value)),_,_) => s"$LIT$value"
      case DFAny.Const(_,DFSInt.Token(_,Some(value)),_,_) => s"$LIT$value"
      case _ => ref(leftArg)
    }
    val rightArgStr = (member.op, rightArg) match {
      case (_, DFAny.Const(_,DFUInt.Token(_,Some(value)),_,_)) => s"$LIT$value"
      case (_, DFAny.Const(_,DFSInt.Token(_,Some(value)),_,_)) => s"$LIT$value"
      case (_, ra) => ref(ra)
    }
    (leftArg, member.op, revision) match {
      case (_, Op.++, _) => s"{$leftArgStr, $rightArgStr}"
      case _ => s"${leftArgStr.applyBrackets()} $OP$opStr ${rightArgStr.applyBrackets()}"
    }
  }
  def alias(member : DFAny.Alias[_ <: DFAny.Type,_ <: DFAny,_ <: DFAny.Modifier])(implicit printer : Printer) : String = {
    import printer.config._
    val relVal = member.relValRef.get
    val relValStr = ref(relVal)
    member match {
      case toVal : DFAny.Alias.AsIs => toVal match {
        case DFSInt(_) => s"$$$FN signed($relValStr)"
        case _ => s"$$$FN unsigned($relValStr)"
      }
      case DFAny.Alias.BitsWL(dfType, _, _, relWidth, relBitLow, _, _) =>
        if (relWidth == relVal.width.getValue) relValStr
        else {
          val relBitHigh = relBitLow + relWidth - 1
          dfType match {
            case DFBool.Type(false) => s"${relValStr.applyBrackets()}[$LIT$relBitLow]"
            case DFBits.Type(_) => s"${relValStr.applyBrackets()}[$LIT$relBitHigh:$LIT$relBitLow]"
          }
        }
      case DFAny.Alias.Resize(dfType, _, _, _) =>
        dfType match {
          case _ if dfType.width.getValue == relVal.width.getValue => relValStr
          case _ if relVal.width.getValue == 1 => s"{${dfType.width-1}'b0, $relValStr}"
          case _ if dfType.width.getValue == 1 => s"$relValStr[0]"
          case DFUInt.Type(_) | DFBits.Type(_) if dfType.width < relVal.width =>
            s"$relValStr[${dfType.width}:0]"
          case DFUInt.Type(_) | DFBits.Type(_) if dfType.width > relVal.width =>
            val extensionWidth = dfType.width - relVal.width
            s"{$extensionWidth'b0, $relValStr[${relVal.width-1}:0]}"
          case DFSInt.Type(width) if width < relVal.width =>
            s"{$relValStr[${relVal.width-1}], $relValStr[${width-2}:0]}"
          case DFSInt.Type(width) if width > relVal.width =>
            val extensionWidth = dfType.width - relVal.width
            s"{{$extensionWidth{$relValStr[${relVal.width-1}]}}, $relValStr[${relVal.width-1}:0]}"
        }
      case _ : DFAny.Alias.Invert => s"$OP!${relValStr.applyBrackets()}"
      case _ : DFAny.Alias.Prev => ??? //should not happen since prev is removed via clocking phase
    }
  }

  def ref(member : DFAny)(implicit printer : Printer) : String = {
    import printer.config._
    member match {
      case c : DFAny.Const => const(c.token)
      case d : DFAny.Dcl => d.getOwnerBlock match {
        case DFDesign.Block.Internal(_,_,_,Some(rep)) => rep match {
          case EdgeDetect.Rep(bitRef, EdgeDetect.Edge.Rising) => s"$OP @posedge(${ref(bitRef)})"
          case EdgeDetect.Rep(bitRef, EdgeDetect.Edge.Falling) => s"$OP @negedge(${ref(bitRef)})"
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
    case f : DFAny.Func2 => func2(f)
    case a : DFAny.Alias[_,_,_] => alias(a)
    case _ : DFAny.Dcl => ??? //shouldn't occur
    case _ : DFAny.Dynamic => ??? //shouldn't occur
    case _ : DFAny.Fork => ??? //shouldn't occur
  }
}

