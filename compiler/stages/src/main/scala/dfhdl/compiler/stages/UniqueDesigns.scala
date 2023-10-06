package dfhdl.compiler.stages

import dfhdl.compiler.analysis.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.patching.*
import dfhdl.internals.*

case object UniqueDesigns extends Stage:
  def dependencies: List[Stage] = List()
  def nullifies: Set[Stage] = Set()
  def transform(designDB: DB)(using MemberGetSet): DB =
    val eqDesign: ((DFDesignBlock, List[DFMember]), (DFDesignBlock, List[DFMember])) => Boolean =
      case ((thisBlock, theseMembers), (thatBlock, thoseMembers))
          if thisBlock.dclMeta == thatBlock.dclMeta =>
        (theseMembers lazyZip thoseMembers).forall { case (l, r) => l =~ r }
      case _ => false
    end eqDesign
    val sameBlockLists: Iterable[List[DFDesignBlock]] =
      designDB.designMemberList.view
        .groupByCompare(eqDesign, _._1.dclName.hashCode()).map(_.unzip._1)
    val uniqueTypeMap: Map[String, Iterable[List[DFDesignBlock]]] =
      sameBlockLists.groupBy(g => g.head.dclName)
    val patchList = uniqueTypeMap.flatMap {
      case (designType, list) if list.size > 1 =>
        list.zipWithIndex.flatMap { case (group, i) =>
          val updatedDclName = s"${designType}_${i.toPaddedString(list.size)}"
          group.map(block =>
            block -> Patch.Replace(
              block.copy(dclMeta = block.dclMeta.copy(nameOpt = Some(updatedDclName))),
              Patch.Replace.Config.FullReplacement
            )
          )
        }
      case _ => Nil
    }.toList
    designDB.patch(patchList)
  end transform
end UniqueDesigns

extension [T: HasDB](t: T)
  def uniqueDesigns: DB =
    StageRunner.run(UniqueDesigns)(t.db)(using dfhdl.options.CompilerOptions.default)
