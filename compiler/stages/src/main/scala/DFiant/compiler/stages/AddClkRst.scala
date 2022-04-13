package DFiant.compiler.stages

import DFiant.compiler.analysis.*
import DFiant.compiler.ir.*
import DFiant.compiler.patching.*

case object AddClkRst extends Stage:
  def dependencies: List[Stage] = List()
  def nullifies: Set[Stage] = Set(ViaConnection)
  def transform(designDB: DB)(using MemberGetSet): DB =
    val patchList: List[(DFMember, Patch)] = designDB.namedOwnerMemberList.flatMap {
      case (owner: (DFDomainOwner & DFBlock), members) =>
        None
      case _ => None
    }
    designDB.patch(patchList)

extension [T: HasDB](t: T) def addClkRst: DB = StageRunner.run(AddClkRst)(t.db)
