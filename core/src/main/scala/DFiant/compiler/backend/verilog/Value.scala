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
      case DFEnum.Token(_, entry) => EnumEntriesDcl.enumEntryRefName(entry.get)
//      case DFVector.Token(_, value) => //this syntax is only relevant for SystemVerilog
//        value.map(const(_)).mkString("'{", ", ", "}")
      case t =>
        println(t)
        ???
    }
  }
  def func1(member : DFAny.Func1)(implicit printer : Printer) : String = {
    import printer.config._
    val leftArg = member.leftArgRef.get
    val opStr = member.op match {
      case DFAny.Func1.Op.unary_! => "!"
      case DFAny.Func1.Op.unary_~ => "~"
      case DFAny.Func1.Op.unary_- => "-"
      case op => ???
    }
    val leftArgStr = leftArg match {
      case _ => ref(leftArg)
    }
    s"$OP$opStr${leftArgStr.applyBrackets()}"
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
  def alias(member : DFAny.Alias)(implicit printer : Printer) : String = {
    import printer.config._
    val relVal = member.relValRef.get
    val relValStr = ref(relVal)
    member match {
      case DFAny.Alias.AsIs(dfType,_,_,_,_) => dfType match {
        case _ if relVal.width == 1 => s"{${dfType.width-1}'b0, $relValStr}"
        case _ if dfType.width.getValue == 1 => s"$relValStr[0]"
        case DFUInt.Type(width) if width.getValue == relVal.width =>
          relVal.dfType match {
            case DFUInt.Type(_) => relValStr
            case _ => s"$$$FN unsigned($relValStr)"
          }
        case DFUInt.Type(_) | DFBits.Type(_) if dfType.width < relVal.width =>
          s"$relValStr[${dfType.width-1}:0]"
        case DFUInt.Type(_) | DFBits.Type(_) if dfType.width > relVal.width =>
          val extensionWidth = dfType.width - relVal.width
          s"{$extensionWidth'b0, $relValStr[${relVal.width-1}:0]}"
        case DFSInt.Type(width) if width.getValue == relVal.width =>
          relVal.dfType match {
            case DFSInt.Type(_) => relValStr
            case _ => s"$$$FN signed($relValStr)"
          }
        case DFSInt.Type(width) if width < relVal.width =>
          s"{$relValStr[${relVal.width-1}], $relValStr[${width-2}:0]}"
        case DFSInt.Type(width) if width > relVal.width =>
          val extensionWidth = dfType.width - relVal.width
          if (extensionWidth.getValue == 1)
            s"{$relValStr[${relVal.width-1}], $relValStr[${relVal.width-1}:0]}"
          else
            s"{{$extensionWidth{$relValStr[${relVal.width-1}]}}, $relValStr[${relVal.width-1}:0]}"
      }
      case DFAny.Alias.BitsWL(dfType, _, _, relWidth, relBitLow, _, _) =>
        if (relWidth == relVal.width) relValStr
        else {
          val relBitHigh = relBitLow + relWidth - 1
          dfType match {
            case DFBool.Type(false) => s"${relValStr.applyBrackets()}[$LIT$relBitLow]"
            case DFBits.Type(_) => s"${relValStr.applyBrackets()}[$LIT$relBitHigh:$LIT$relBitLow]"
          }
        }
      case _ : DFAny.Alias.Prev => ??? //should not happen since prev is removed via clocking phase
    }
  }

  def ref(member : DFAny.Member)(implicit printer : Printer) : String = {
    import printer.config._
    member match {
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
  def apply(member : DFAny.Member)(implicit printer : Printer) : String = member match {
    case c : DFAny.Const => const(c.token)
    case f : DFAny.Func1 => func1(f)
    case f : DFAny.Func2 => func2(f)
    case a : DFAny.Alias => alias(a)
    case DFAny.ApplySel(_, _, relValRef,idxRef, _, _) =>
      s"${Value.ref(relValRef).applyBrackets()}[${Value.ref(idxRef)}]"
    case _ : DFAny.Dcl => ??? //shouldn't occur
    case _ : DFAny.Dynamic => ??? //shouldn't occur
    case _ : DFAny.Fork => ??? //shouldn't occur
  }
}

