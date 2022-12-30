package dfhdl.compiler.stages

import dfhdl.compiler.analysis.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.patching.*
import dfhdl.internals.*

private final class UniqueBlock(val block: DFDesignBlock, val members: List[DFMember])(using
    MemberGetSet
):
  override def equals(obj: Any): Boolean = obj match
    case that: UniqueBlock
        if this.block.dclName == that.block.dclName && this.block.dclPosition == that.block.dclPosition =>
      (this.members lazyZip that.members).forall {
        case (l: DFNet, r: DFNet) if l.isViaConnection && r.isViaConnection => true
        case (l, r)                                                           => l =~ r
      }
    case _ => false
  override def hashCode(): Int = block.dclName.hashCode

case object UniqueDesigns extends Stage:
  def dependencies: List[Stage] = List()
  def nullifies: Set[Stage] = Set()
  def transform(designDB: DB)(using MemberGetSet): DB =
    val uniqueBlockMap: Map[UniqueBlock, List[DFDesignBlock]] =
      designDB.designMemberList.view
        .groupBy((design, members) => new UniqueBlock(design, members))
        .view
        .mapValues(_.map(_._1).toList)
        .toMap
    val uniqueTypeMap: Map[String, List[UniqueBlock]] =
      uniqueBlockMap.keys.toList.groupBy(ub => ub.block.dclName)
    val patchList = uniqueTypeMap.flatMap {
      case (designType, list) if list.size > 1 =>
        list.zipWithIndex.flatMap { case (ub, i) =>
          val updatedDclName = s"${designType}_${i.toPaddedString(list.size)}"
          uniqueBlockMap(ub).map(block =>
            block -> Patch.Replace(
              block.copy(dclName = updatedDclName),
              Patch.Replace.Config.FullReplacement
            )
          )
        }
      case _ => Nil
    }.toList
    designDB.patch(patchList)
  end transform
end UniqueDesigns

extension [T: HasDB](t: T) def uniqueDesigns: DB = StageRunner.run(UniqueDesigns)(t.db)
