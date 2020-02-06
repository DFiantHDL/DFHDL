package ZFiant

import DFiant.internals._
import DFCompiler.Utils

final class CodeString[C](c : C)(implicit comp : Compilable[C]) {
  private val designDB = comp(c)
  private val fixedDB = designDB.fixAnonymous
  import fixedDB.getset

  def blockBodyCodeString(block : DFBlock, members : List[DFMember], lateConstruction : Boolean)(implicit printConfig : CodeString.Config) : String = {
    val membersCodeString = members.flatMap {
      case m if m.hasLateConstruction != lateConstruction => None
      case mh : ConditionalBlock.MatchHeader => Some(mh.codeString)
      case cb : ConditionalBlock => Some(cb.codeString(blockBodyCodeString(cb, fixedDB.ownerMemberTable(cb), lateConstruction)))
      case DFDesign.Block.Internal(_,_,Some(_)) => None
      case d : DFDesign.Block =>
        val body = blockBodyCodeString(d, designDB.ownerMemberTable(d), lateConstruction = true)
        val bodyBrackets = if (body == "") "{}" else s"{\n${body.delimRowsBy(CodeString.delim)}\n}"
        Some(s"final val ${d.name} = new ${d.typeName} $bodyBrackets") //TODO: fix
      case n : DFNet => n.toRef.get.getOwner match {
        case DFDesign.Block.Internal(_,_,Some(_)) => None //ignoring inlined block connection
        case _ => Some(n.codeString)
      }
      case a : DFAny if !a.isAnonymous =>
        val initInfo = printConfig match {
          case CodeString.Config.Default => ""
          case CodeString.Config.ShowInits => a.tags.init match {
            case Some(init) => s"//init = ${init.codeString}"
            case None => "//init = Unknown"
          }
        }
        Some(s"final val ${a.name} = ${a.codeString}$initInfo")
      case _ => None
    }
    membersCodeString.mkString("\n")
  }
  def codeString(implicit printConfig : CodeString.Config) : String = {
    val bodyDB = new DSLOwnerConstruct.DB[DFBlock, String]{
      override def ownerToString(ownerTypeName: String, ownerBody: String): String =
        s"trait $ownerTypeName extends DFDesign {\n${ownerBody.delimRowsBy(CodeString.delim)}\n}"
    }
    fixedDB.ownerMemberList.foreach {
      case (DFDesign.Block.Internal(_,_,Some(_)), _) =>
      case (block : DFDesign.Block, members) =>
        bodyDB.addOwnerBody(block.typeName, blockBodyCodeString(block, members, lateConstruction = false), block)
      case _ =>
    }
    bodyDB.dbString
  }
  def printCodeString()(implicit printConfig : CodeString.Config) : DFDesign.DB = {
    println(codeString)
    fixedDB
  }
}

object CodeString {
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