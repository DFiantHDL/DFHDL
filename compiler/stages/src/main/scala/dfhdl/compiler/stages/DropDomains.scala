package dfhdl.compiler.stages

import dfhdl.compiler.analysis.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.patching.*
import dfhdl.options.CompilerOptions
import annotation.FlattenMode

/** This stage flattens the domains by removing them and changing their named members according to
  * the flattening mode.
  */
case object DropDomains extends HierarchyStage:
  def dependencies: List[Stage] = List(ToED)
  def nullifies: Set[Stage] = Set(DFHDLUniqueNames, SimpleOrderMembers)
  def transformSubDB(rootDB: DB)(using MemberGetSet, CompilerOptions, RefGen): DB =
    // the flattened name of a member owned by `domain`, applying the name flattening mode
    // of each domain in the composition chain until reaching a non-domain owner
    def flattenedName(name: String, domain: DomainBlock)(using MemberGetSet): String =
      var currentDomain: DomainBlock = domain
      var currentName = name
      var inDomain = true
      while (inDomain)
        currentDomain.flattenMode match
          case FlattenMode.Transparent => // no change
          case FlattenMode.Prefix(sep) =>
            currentName = s"${currentDomain.getName}$sep$currentName"
          case FlattenMode.Suffix(sep) =>
            currentName = s"${currentName}$sep${currentDomain.getName}"
        currentDomain.getOwner match
          case domain: DomainBlock => currentDomain = domain
          case _                   => inDomain = false
      currentName
    end flattenedName
    val patchList = subDB.membersNoGlobals.flatMap {
      // all domains are removed and their members referencing them need to point to the owner design
      case domain: DomainBlock =>
        Some(
          domain -> Patch.Replace(domain.getOwnerDesign, Patch.Replace.Config.ChangeRefAndRemove)
        )
      // ignore design block members
      case designBlock: DFDesignBlock => None
      // a by-name selection of a domain-nested port (e.g. a related domain's derived clock,
      // selected as `active.clk`) must follow the port's flattened name in the child design
      case pbns: DFVal.PortByNameSelect if pbns.portNamePath.contains('.') =>
        rootDB.pbnsToPort(pbns, subDB).flatMap { case (dcl, childSub) =>
          val flatName = childSub.atGetSet {
            dcl.getOwner match
              case domain: DomainBlock => flattenedName(dcl.getName, domain)
              case _                   => dcl.getName
          }
          if (flatName == pbns.portNamePath) None
          else
            Some(
              pbns -> Patch.Replace(
                pbns.copy(portNamePath = flatName),
                Patch.Replace.Config.FullReplacement
              )
            )
        }
      // named members owned by domains could need to change their name depending on the flattening mode
      // of its domain owner chain
      case member: DFMember.Named if !member.isAnonymous =>
        member.getOwner match
          case domain: DomainBlock =>
            val currentName = flattenedName(member.getName, domain)
            // when all domains are transparent then there is no name change
            if (currentName != member.getName)
              Some(
                member -> Patch.Replace(
                  member.setName(currentName),
                  Patch.Replace.Config.FullReplacement
                )
              )
            else None
          case _ => None
      case _ => None
    }
    subDB.patch(patchList)
  end transformSubDB
end DropDomains

extension [T: HasDB](t: T)
  def dropDomains(using CompilerOptions): DB =
    StageRunner.run(DropDomains)(t.db)
