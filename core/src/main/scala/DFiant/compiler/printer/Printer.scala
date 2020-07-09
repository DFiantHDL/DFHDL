package DFiant
package compiler
package printer

import DFiant.internals._
import DFiant.sim._

import collection.mutable

final class PrinterOps[D <: DFDesign, C](c : C)(implicit conv : C => Compilation[D]) {
  private val fixedDB = conv(c).flattenNames.fixAnonymous.uniqueNames(Set(), caseSensitive = true).uniqueDesigns.db
  import fixedDB.__getset

  private def blockBodyCodeString(owner : DFOwner, members : List[DFMember], lateConstruction : Boolean)(
    implicit printConfig : Printer.Config
  ) : String = {
    import printConfig._
    import formatter._
    val finalStr = owner match {
      case _ : ConditionalBlock => "" //local values cannot be annotated as "final"
      case _ => s"$SC final "
    }
    val membersCodeString = members.flatMap {
      case m if m.hasLateConstruction != lateConstruction => None
      case mh : ConditionalBlock.MatchHeader => Some(mh.codeString)
      case cb : ConditionalBlock => Some(cb.codeString(blockBodyCodeString(cb, fixedDB.blockMemberTable(cb), lateConstruction)))
      case DFDesign.Block.Internal(_,_,_,Some(_)) => None
      case d : DFDesign.Block =>
        val body = blockBodyCodeString(d, fixedDB.blockMemberTable(d), lateConstruction = true)
        val bodyBrackets = if (body == "") "{}" else s"{\n${body.delim()}\n}"
        Some(s"$finalStr$SC val ${d.name} ${ALGN(0)}= $SC new ${d.typeName} $bodyBrackets") //TODO: fix
      case n : DFNet => n.toRef.getOwnerBlock match {
        case DFDesign.Block.Internal(_,_,_,Some(_)) => None //ignoring inlined block connection
        case _ => Some(n.codeString)
      }
      case dc @ DFAny.Dynamic(_,_ : DFAny.Dynamic.Func.Control,_,_) => Some(dc.codeString)
      case sim : DFSimMember => Some(sim.codeString)
      case emitter : BackendEmitter => Some(emitter.codeString)
      case a : DFAny if !a.isAnonymous =>
        val initInfo = if (printConfig.showInits) a.tags.init match {
          case Some(init) => s"//init = ${init.codeString}"
          case None => "//init = Unknown"
        } else ""
        val customTagInfo =
          if (printConfig.showCustomTags && a.tags.customTags.nonEmpty)
            a.tags.customTags.mkString(s" ${DF}!! ", s" ${DF}!! ", "")
          else ""
        Some(s"$finalStr$SC val ${a.name} ${ALGN(0)}= ${a.codeString}$customTagInfo$initInfo")
      case _ => None
    }
    membersCodeString.mkString("\n")
  }
  private def designBlockCodeString(block : DFDesign.Block, members : List[DFMember])(
    implicit printConfig : Printer.Config
  ) : String = {
    import printConfig._
    import formatter._
    val localEnumString = fixedDB.getLocalEnumTypes(block).map(e => e.codeString).mkString("","\n","\n")
    val body = localEnumString + blockBodyCodeString(block, members, lateConstruction = false)
    val classStr = block match {
      case DFDesign.Block.Top(_, _, DFSimDesign.Mode.On) => "DFSimDesign"
      case _ => "DFDesign"
    }
    s"$SC trait ${block.designType} $SC extends $DF $classStr {\n${body.delim()}\n}"
  }
  def codeString(implicit printConfig : Printer.Config) : String = {
    import printConfig._
    import formatter._
    val uniqueDesigns = mutable.Set.empty[String]
    val globalEnumString = fixedDB.getGlobalEnumTypes.map(e => e.codeString)
    val codeStringList = fixedDB.blockMemberList.flatMap {
      case (DFDesign.Block.Internal(_,_,_,Some(_)), _) => None
      case (block : DFDesign.Block, members) if !uniqueDesigns.contains(block.designType) =>
        uniqueDesigns += block.designType
        Some(designBlockCodeString(block, members))
      case _ => None
    }
    (globalEnumString ++ codeStringList).mkString(s"\n$EMPTY\n").formatted
  }
  def printCodeString(implicit printConfig : Printer.Config) : C = {
    println(codeString)
    c
  }
}

sealed trait Printer {
  val getSet : MemberGetSet
  val config : Printer.Config
}

object Printer {
  implicit def ev(implicit cfg: Config, gs: MemberGetSet) : Printer = new Printer {
    val getSet: MemberGetSet = gs
    val config: Config = cfg
  }

  sealed trait Config {
    import io.AnsiColor._
    val showCustomTags : Boolean = true
    val showInits : Boolean = false
    val DELIM : String = "  "
    val LIT : String = BLUE
    val STR : String = s"\u001B[38;5;34m$BOLD"
    val SC : String = s"$BLUE$BOLD"
    val DF : String = s"\u001B[38;5;92m$BOLD"
    val TP : String = "\u001B[38;5;94m"
    val CMT : String = "\u001B[38;5;247m"
    val formatter : Formatter = new Formatter("  ", List(25, 25))
  }
  object Config {
    implicit case object Default extends Config
    case object ShowInits extends Config {
      override val showInits: Boolean = true
    }
  }
}