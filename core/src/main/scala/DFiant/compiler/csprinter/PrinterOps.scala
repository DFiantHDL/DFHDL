package DFiant
package compiler
package csprinter

import printer.formatter._
import DFiant.sim._

import scala.collection.mutable

final class PrinterOps[D <: DFDesign, C](c: C)(implicit
    conv: C => Compilation[D]
) {
  private val fixedDB =
    conv(c).dropUnreferenced.fixAnonymous.flattenNames
      .uniqueNames(Set(), caseSensitive = true)
      .uniqueDesigns
      .db
  import fixedDB.__getset

  private def blockBodyCodeString(
      owner: DFOwner,
      members: List[DFMember],
      lateConstruction: Boolean
  )(implicit
      printer: CSPrinter
  ): String = {
    import printer.config._
    val finalStr = owner match {
      case _: DFConditional.Block =>
        "" //local values cannot be annotated as "final"
      case _ => s"$SC final "
    }
    val membersCodeString = members.flatMap {
      case m if m.hasLateConstruction != lateConstruction => None
      case mh: DFConditional.MatchHeader                  => Some(mh.codeString)
      case cb: DFConditional.Block =>
        Some(
          cb.codeString(
            blockBodyCodeString(
              cb,
              fixedDB.blockMemberTable(cb),
              lateConstruction
            )
          )
        )
      case DFDesign.Block.Internal(_, _, _, Some(_)) => None //inlined block
      case d: DFDesign.Block =>
        d.getTagOf[CompactCodeString] match {
          case Some(ccs) => ccs.codeString(printer, owner)
          case None =>
            val body = blockBodyCodeString(
              d,
              fixedDB.blockMemberTable(d),
              lateConstruction = true
            )
            val bodyBrackets =
              if (body == "") "{}" else s"{\n${body.delim()}\n}"
            Some(
              s"$finalStr$SC val ${d.name} ${ALGN(0)}= $SC new ${d.designType} $bodyBrackets"
            ) //TODO: fix
        }
      case n: DFNet =>
        n.toRef.getOwnerBlock match {
          case DFDesign.Block.Internal(_, _, _, Some(_)) =>
            None //ignoring inlined block connection
          case _ =>
            Some(
              n.codeString
            ) //TODO: maybe CompactCodeString changes this or, alternatively, transparent ports
        }
      case dsnControl: DFDesign.Control => Some(dsnControl.codeString)
      case dc @ DFAny.Dynamic(_, _: DFAny.Dynamic.Func.Control, _, _) =>
        Some(dc.codeString)
      case sim: DFSimMember        => Some(sim.codeString)
      case emitter: BackendEmitter => Some(emitter.codeString)
      case a: DFAny.Member if !a.isAnonymous =>
        val customTagInfo = showTagsFilter match {
          case Some(tagsFilter) =>
            val visibleTags = a.tags.customTags.values.filter(tagsFilter)
            if (visibleTags.nonEmpty)
              visibleTags.mkString(s" ${DF}tag ", s" ${DF}tag ", "")
            else ""
          case None => ""
        }
        Some(
          s"$finalStr$SC val ${a.name} ${ALGN(0)}= ${a.codeString}$customTagInfo"
        )
      case _ => None
    }
    membersCodeString.mkString("\n")
  }
  private def designBlockCodeString(
      block: DFDesign.Block,
      members: List[DFMember]
  )(implicit
      printer: CSPrinter
  ): String =
    block.getTagOf[CompactCodeString] match {
      case Some(_) =>
        "" //If a block has a compact codestring representation then we don't display its declaration
      case None =>
        import printer.config._
        val localEnumString = fixedDB
          .getLocalEnumTypes(block)
          .map(e => e.codeString)
          .mkString("", "\n", "\n")
        val body = localEnumString + blockBodyCodeString(
          block,
          members,
          lateConstruction = false
        )
        val classStr = block match {
          case DFDesign.Block.Top(_, _, DFSimDesign.Mode.On) => "DFSimDesign"
          case _                                             => "DFDesign"
        }
        s"$DF@df $SC class ${block.designType} $SC extends $DF $classStr {\n${body.delim()}\n}"
    }

  /**
    * @return a string DFiant code representation of the design
    */
  def codeString(implicit printConfig: CSPrinter.Config): String = {
    implicit val printer: CSPrinter = new CSPrinter {
      val getSet: MemberGetSet     = __getset
      val config: CSPrinter.Config = printConfig
    }
    val uniqueDesigns    = mutable.Set.empty[String]
    val globalEnumString = fixedDB.getGlobalEnumTypes.map(e => e.codeString)
    val codeStringList = fixedDB.blockMemberList.flatMap {
      case (DFDesign.Block.Internal(_, _, _, Some(_)), _) => None
      case (block: DFDesign.Block, members)
          if !uniqueDesigns.contains(block.designType) =>
        uniqueDesigns += block.designType
        Some(designBlockCodeString(block, members))
      case _ => None
    }
    (globalEnumString ++ codeStringList).mkString(s"\n$EMPTY\n").formatted
  }

  /**
    * Prints out a DFiant code representation of the design
    * @return this unmodified compilation context / design
    */
  def printCodeString(implicit printConfig: CSPrinter.Config): C = {
    println(codeString)
    c
  }
}
