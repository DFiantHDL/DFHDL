package ZFiant
package compiler
package printer

import DFiant.internals._
import collection.mutable

private object Formating {
  import io.AnsiColor._
  final val LIT = BLUE
  final val SC = s"$BLUE$BOLD"
  final val DF = "\u001B[38;5;94m"
  final val ALGN1 = "$$1$$"
  final val ALGN1_MAX = 20
  final val ALGN2 = "$$2$$"
  final val ALGN2_MAX = 20
  private val colorCode = "\u001B\\[[;\\d]*m"
  private val optionalSpaces = "[ ]*"
  private val word = "([0-9a-zA-Z_]+)"
  private val operator = "([<>+\\-*/=:!^&%|]+)"
  private val string = """(".*")"""
  private val noreset = "\u001B{0}"
  private val coloredSymbol = s"($colorCode)$optionalSpaces(($word|$operator|$string){1})$noreset".r.unanchored
  implicit class ColoringString(text : String) {
    def colored : String = coloredSymbol.replaceAllIn(text, m => s"${m.group(1)}${m.group(2)}$RESET")
    def uncolor : String = text.replaceAll(colorCode, "")
    def aligned : String = {
      val uncolored = text.uncolor
      val posList : List[Int] = uncolored.linesIterator.map(l => l.indexOf(ALGN1)).toList
      val maxPos = posList.max
      val addedSpaceList = posList.map {
        case i if i >= 0 => (maxPos - i) min ALGN1_MAX
        case _ => 0
      }
      (text.linesIterator zip addedSpaceList).map{case (line, space) => line.replace(ALGN1, " "*space)}.mkString("\n")
    }
  }
}

import Formating._
final class PrinterOps[D <: DFDesign, S <: shapeless.HList](c : Compilable[D, S]) {
  private val fixedDB = c.fixAnonymous.uniqueNames(Set(), caseSensitive = true).uniqueDesigns.db
  import fixedDB.__getset

  private def blockBodyCodeString(members : List[DFMember], lateConstruction : Boolean)(implicit printConfig : Printer.Config) : String = {
    val membersCodeString = members.flatMap {
      case m if m.hasLateConstruction != lateConstruction => None
      case mh : ConditionalBlock.MatchHeader => Some(mh.codeString)
      case cb : ConditionalBlock => Some(cb.codeString(blockBodyCodeString(fixedDB.ownerMemberTable(cb), lateConstruction)))
      case DFDesign.Block.Internal(_,_,_,Some(_)) => None
      case d : DFDesign.Block =>
        val body = blockBodyCodeString(fixedDB.ownerMemberTable(d), lateConstruction = true)
        val bodyBrackets = if (body == "") "{}" else s"{\n${body.delimRowsBy(Printer.delim)}\n}"
        Some(s"$SC final $SC val ${d.name} $ALGN1= $SC new ${d.typeName} $bodyBrackets") //TODO: fix
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
        Some(s"$SC final $SC val ${a.name} $ALGN1= ${a.codeString}$initInfo")
      case _ => None
    }
    membersCodeString.mkString("\n")
  }
  private def designBlockCodeString(block : DFDesign.Block, members : List[DFMember])(implicit printConfig : Printer.Config) : String = {
    val body = blockBodyCodeString(members, lateConstruction = false)
    s"$SC trait ${block.designType} $SC extends $DF DFDesign {\n${body.delimRowsBy(Printer.delim)}\n}"
  }
  def codeString(implicit printConfig : Printer.Config) : String = {
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
  val delim : String = "  "
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

  sealed trait Config
  object Config {
    implicit case object Default extends Config
    case object ShowInits extends Config
  }
}