package DFiant
package compiler
package printer

import DFiant.internals._
import DFiant.sim._

import collection.mutable

final class PrinterOps[D <: DFDesign, S <: shapeless.HList](c : Compilable[D, S]) {
  private val fixedDB = c.flattenNames.fixAnonymous.uniqueNames(Set(), caseSensitive = true).uniqueDesigns.db
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
    val body = blockBodyCodeString(block, members, lateConstruction = false)
    val classStr = block match {
      case DFDesign.Block.Top(_, _, DFSimulator.Mode.On) => "DFSimulator"
      case _ => "DFDesign"
    }
    s"$SC trait ${block.designType} $SC extends $DF $classStr {\n${body.delim()}\n}"
  }
  def codeString(implicit printConfig : Printer.Config) : String = {
    import printConfig._
    import formatter._
    val uniqueDesigns = mutable.Set.empty[String]
    val codeStringList = fixedDB.blockMemberList.flatMap {
      case (DFDesign.Block.Internal(_,_,_,Some(_)), _) => None
      case (block : DFDesign.Block, members) if !uniqueDesigns.contains(block.designType) =>
        uniqueDesigns += block.designType
        Some(designBlockCodeString(block, members))
      case _ => None
    }
    codeStringList.mkString(s"\n$EMPTY\n").formatted
  }
  def printCodeString()(implicit printConfig : Printer.Config) : Compilable[D, S] = {
    println(codeString)
    c
  }
  def printGenFiles()(implicit printConfig : Printer.Config) : Compilable[D, S] = {
    import printConfig.formatter._
    c.cmdSeq.foreach{
      case Compilable.Cmd.GenFile(fileName, contents) => println(
        s"""\n\n@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
           |@ Contents of $fileName
           |@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
           |$contents
           |""".stripMargin
      )
    }
    c
  }
  def toFolder(folderName : String)(implicit printConfig : Printer.Config) : Compilable[D, S] = {
    import java.io._
    import printConfig.formatter._
    new File(folderName).mkdirs()
    //writing entity and architecture files
    c.cmdSeq.foreach{
      case Compilable.Cmd.GenFile(fileName, contents) =>
        val uncolored = contents.uncolor
        val pw = new FileWriter(new File(s"$folderName/$fileName"))
        pw.write(uncolored)
        pw.close()
    }
    c
  }
  def toFile(fileName : String)(implicit printConfig : Printer.Config) : Compilable[D, S] = {
    import java.io._
    import printConfig.formatter._
    //writing entity and architecture files
    val pw = new FileWriter(new File(s"$fileName"))
    c.cmdSeq.foreach{
      case Compilable.Cmd.GenFile(_, contents) =>
        val uncolored = contents.uncolor
        pw.write(uncolored)
    }
    pw.close()
    c
  }
}

object Printer {
  trait Context {
    val callOwner : DFBlock
    val getSet : MemberGetSet
  }
  object Context {
    implicit def evContext(implicit ctx : DFMember.Context) : Context = new Context {
      override val callOwner: DFBlock = ctx.owner match {
        case b : DFBlock => b
        case o => o.getOwnerBlock
      }
      override val getSet: MemberGetSet = ctx.db.getSet
    }
    implicit def ev(implicit co : DFBlock, gs : MemberGetSet, lp : shapeless.LowPriority) : Context = new Context {
      override val callOwner: DFBlock = co
      override val getSet: MemberGetSet = gs
    }
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
    val formatter : Formatter = new Formatter("  ", List(25, 25))
  }
  object Config {
    implicit case object Default extends Config
    case object ShowInits extends Config {
      override val showInits: Boolean = true
    }
  }
}