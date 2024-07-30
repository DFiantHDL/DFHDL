package dfhdl.compiler.stages

import dfhdl.compiler.analysis.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.patching.*
import dfhdl.options.CompilerOptions
import dfhdl.internals.*

case object UniqueDesigns extends Stage:
  def dependencies: List[Stage] = List()
  def nullifies: Set[Stage] = Set()
  def transform(designDB: DB)(using MemberGetSet, CompilerOptions): DB =
    val eqDesign: ((DFDesignBlock, List[DFMember]), (DFDesignBlock, List[DFMember])) => Boolean =
      case ((thisBlock, theseMembers), (thatBlock, thoseMembers))
          if thisBlock.dclMeta == thatBlock.dclMeta =>
        (theseMembers lazyZip thoseMembers).forall { case (l, r) => l =~ r }
      case _ => false
    end eqDesign
    // we're grouping always according to case-insensitive design names because these affect
    // the eventual file names and we want these to be different across all operating systems.
    // the actual name case is preserved for design/entity/module generation.
    val sameBlockLists: List[List[DFDesignBlock]] =
      designDB.uniqueDesignMemberList.view
        .groupByCompare(eqDesign, _._1.dclName.toLowerCase().hashCode()).map(_.unzip._1).toList
    val uniqueTypeMap: Map[String, List[List[DFDesignBlock]]] =
      sameBlockLists.groupBy(g => g.head.dclName.toLowerCase())
    val patchList = uniqueTypeMap.flatMap { case (designType, list) =>
      list.zipWithIndex.flatMap {
        case (group, i) if group.length > 1 || list.length > 1 =>
          val groupHead = group.head
          // using the actual name and not the `designType` grouping, to preserve the original
          // naming. we only lower-cased it for case-insensitive grouping.
          val updatedDclName =
            if (list.length > 1)
              if (groupHead.isTop) groupHead.dclName // top name should not be mangled
              else s"${groupHead.dclName}_${i.toPaddedString(list.length)}"
            else groupHead.dclName
          var first = true
          group.map(block =>
            val tags =
              if (first)
                first = false
                block.tags
              else block.tags.tag(DuplicateTag)
            block -> Patch.Replace(
              block.copy(dclMeta = block.dclMeta.copy(nameOpt = Some(updatedDclName)), tags = tags),
              Patch.Replace.Config.FullReplacement
            )
          )
        case _ => None
      }
    }.toList
    designDB.patch(patchList)
  end transform
end UniqueDesigns

extension [T: HasDB](t: T)
  def uniqueDesigns(using CompilerOptions): DB =
    StageRunner.run(UniqueDesigns)(t.db)
