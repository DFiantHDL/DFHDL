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
    val sameBlockLists: List[List[DFDesignBlock]] =
      designDB.uniqueDesignMemberList.view
        .groupByCompare(eqDesign, _._1.dclName.hashCode()).map(_.unzip._1).toList
    val uniqueTypeMap: Map[String, List[List[DFDesignBlock]]] =
      sameBlockLists.groupBy(g => g.head.dclName)
    val patchList = uniqueTypeMap.flatMap { case (designType, list) =>
      list.zipWithIndex.flatMap {
        case (group, i) if group.length > 1 || list.length > 1 =>
          val updatedDclName =
            if (list.length > 1) s"${designType}_${i.toPaddedString(list.length)}"
            else designType
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
