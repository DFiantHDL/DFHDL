package DFiant.compiler.stages

import DFiant.compiler.analysis.*
import DFiant.compiler.ir.*
import DFiant.compiler.patching.*

case object ExplicitClkRstCfg extends Stage:
  def dependencies: List[Stage] = List()
  def nullifies: Set[Stage] = Set()
  def transform(designDB: DB)(using MemberGetSet): DB =
    val patchList: List[(DFMember, Patch)] = designDB.namedOwnerMemberList.flatMap {
      case (owner: (DFDomainOwner & DFBlock), members) =>
        owner.domainType match
          case domainType: DomainType.RT =>
            None
          case _ => None
      case _ => None
    }
    designDB.patch(patchList)

extension [T: HasDB](t: T) def explicitClkRstCfg: DB = StageRunner.run(ExplicitClkRstCfg)(t.db)
