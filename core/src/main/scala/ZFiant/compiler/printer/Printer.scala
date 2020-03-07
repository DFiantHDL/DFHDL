package ZFiant
package compiler
package printer

import DFiant.internals._
import collection.mutable

final class PrinterOps[D <: DFDesign, S <: shapeless.HList](c : Compilable[D, S]) {
  private val fixedDB = c.fixAnonymous.uniqueNames(Set(), caseSensitive = true).uniqueDesigns.db
  import fixedDB.__getset

  private def blockBodyCodeString(members : List[DFMember], lateConstruction : Boolean)(implicit printConfig : Printer.Config) : String = {
    import printConfig._
    val membersCodeString = members.flatMap {
      case m if m.hasLateConstruction != lateConstruction => None
      case mh : ConditionalBlock.MatchHeader => Some(mh.codeString)
      case cb : ConditionalBlock => Some(cb.codeString(blockBodyCodeString(fixedDB.ownerMemberTable(cb), lateConstruction)))
      case DFDesign.Block.Internal(_,_,_,Some(_)) => None
      case d : DFDesign.Block =>
        val body = blockBodyCodeString(fixedDB.ownerMemberTable(d), lateConstruction = true)
        val bodyBrackets = if (body == "") "{}" else s"{\n${body.delimRowsBy(DELIM)}\n}"
        Some(s"$SC final $SC val ${d.name} ${ALGN(0)}= $SC new ${d.typeName} $bodyBrackets") //TODO: fix
      case n : DFNet => n.toRef.get.getOwner match {
        case DFDesign.Block.Internal(_,_,_,Some(_)) => None //ignoring inlined block connection
        case _ => Some(n.codeString)
      }
      case a : DFAny if !a.isAnonymous =>
        val initInfo = printConfig match {
          case Printer.Config.Default => ""
          case Printer.Config.ShowInits => a.tags.init match {
            case Some(init) => s"//init = ${init.codeString}"
            case None => "//init = Unknown"
          }
        }
        Some(s"$SC final $SC val ${a.name} ${ALGN(0)}= ${a.codeString}$initInfo")
      case _ => None
    }
    membersCodeString.mkString("\n")
  }
  private def designBlockCodeString(block : DFDesign.Block, members : List[DFMember])(implicit printConfig : Printer.Config) : String = {
    import printConfig._
    val body = blockBodyCodeString(members, lateConstruction = false)
    s"$SC trait ${block.designType} $SC extends $DF DFDesign {\n${body.delimRowsBy(DELIM)}\n}"
  }
  def codeString(implicit printConfig : Printer.Config) : String = {
    import printConfig._
    import formatter._
    val uniqueDesigns = mutable.Set.empty[String]
    val codeStringList = fixedDB.ownerMemberList.flatMap {
      case (DFDesign.Block.Internal(_,_,_,Some(_)), _) => None
      case (block : DFDesign.Block, members) if !uniqueDesigns.contains(block.designType) =>
        uniqueDesigns += block.designType
        Some(designBlockCodeString(block, members))
      case _ => None
    }
    codeStringList.mkString("\n").colored.aligned
  }
  def printCodeString()(implicit printConfig : Printer.Config) : Compilable[D, S] = {
    println(codeString)
    c
  }
  def printGenFiles()(implicit printConfig : Printer.Config) : Compilable[D, S] = {
    c.cmdSeq.foreach{
      case Compilable.Cmd.GenFile(fileName, contents) => println(
        s"""@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
           |@ Contents of $fileName
           |@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
           |$contents
           |""".stripMargin
      )
    }
    c
  }
}

object Printer {
  trait Context {
    val callOwner : DFBlock
    val getset : MemberGetSet
  }
  object Context {
    implicit def evContext(implicit ctx : DFMember.Context) : Context = new Context {
      override val callOwner: DFBlock = ctx.owner
      override val getset: MemberGetSet = ctx.db.getset
    }
    implicit def ev(implicit co : DFBlock, gs : MemberGetSet, lp : shapeless.LowPriority) : Context = new Context {
      override val callOwner: DFBlock = co
      override val getset: MemberGetSet = gs
    }
  }

  sealed trait Config {
    import io.AnsiColor._
    val DELIM : String = "  "
    val LIT : String = BLUE
    val STR : String = s"\u001B[38;5;34m$BOLD"
    val SC : String = s"$BLUE$BOLD"
    val DF : String = s"\u001B[38;5;92m$BOLD"
    val TP : String = "\u001B[38;5;94m"
    val formatter : Formatter = new Formatter(List(25, 25))
    def ALGN(idx : Int) : String = formatter.ALGN(idx)
  }
  object Config {
    implicit case object Default extends Config
    case object ShowInits extends Config
  }
}