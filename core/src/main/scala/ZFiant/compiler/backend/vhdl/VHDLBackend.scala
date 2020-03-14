package ZFiant
package compiler
package backend
package vhdl

import ZFiant.DFAny.Func2.Op
import backend.utils._
import compiler.sync._
import DFiant.internals.StringExtras
import ZFiant.compiler.backend.vhdl

import scala.collection.mutable

private object Init {
  def apply(member : DFAny)(implicit printer : Printer) : Option[String] = member.tags.init match {
    case Some(token +: Nil) if !token.isBubble => Some(Value.const(token))
    case _ => None
  }
}

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
    import printer.config.formatter._
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

final class VHDLBackend[D <: DFDesign, S <: shapeless.HList](c : Compilable[D, S]) {
  private val designDB =
    c.explicitPrev
     .explicitConversions
     .viaPortConnection
     .uniqueDesigns
     .uniqueNames(reservedKeywords, caseSensitive = false)
     .clockedPrev
     .db

  import designDB.__getset

  def compile = {
    val designTypes = mutable.Set.empty[String]
    val files = designDB.designMemberList.flatMap {
      case (design : DFDesign.Block.Internal, _) if design.inlinedRep.nonEmpty => None
      case (design, members) if !designTypes.contains(design.designType) =>
        designTypes += design.designType
        val ports = members.collect {
          case p @ DFAny.Port.In() => syntax.ValueDcl(syntax.ValueDcl.Modifier.Port.In, p.name, Type(p), Init(p)).toString
          case p @ DFAny.Port.Out() => syntax.ValueDcl(syntax.ValueDcl.Modifier.Port.Out, p.name, Type(p), Init(p)).toString
        }
        val entityName = design.designType
        val entity = Entity(entityName, ports)
        val signals = members.flatMap {
          case DFAny.Port.In() | DFAny.Port.Out() => None
          case x : DFAny if designDB.getConnectionTo(x).isDefined || x.tags.customTags.contains(SyncTag.Reg) =>
            Some(syntax.ValueDcl(syntax.ValueDcl.Modifier.Signal, x.name, Type(x), Init(x)).toString)
          case _ => None
        }
        val componentInstances = members.collect {
          case x : DFDesign.Block.Internal if x.inlinedRep.isEmpty =>
            val connections = designDB.designMemberTable(x).collect {
              case net : DFNet.Connection if net.hasLateConstruction =>
                val toVal = net.toRef.get
                val fromVal = net.fromRef.get
                if (toVal.isMemberOfDesign(x)) (toVal.name, fromVal.name) else (fromVal.name, toVal.name)
            }
            ComponentInstance(x.name, x.designType, connections).toString
        }
        val architecture = Architecture(s"${entityName}_arch", entityName, signals, componentInstances)
        val file = syntax.File(entity, architecture)
        Some(Compilable.Cmd.GenFile(s"${design.designType}.vhdl", s"$file"))
      case _ => None
    }
    c.newStage[VHDLCompiler](designDB, c.cmdSeq ++ files)
  }
}

trait VHDLCompiler extends Compilable.Stage