package dfhdl.compiler.stages

import dfhdl.compiler.analysis.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.patching.*
import dfhdl.options.CompilerOptions

/** This stage flattens the domains by removing them and changing their named members according to
  * the flattening mode.
  */
case object DropDomains extends Stage:
  def dependencies: List[Stage] = List(ToED)
  def nullifies: Set[Stage] = Set(DFHDLUniqueNames, SimpleOrderMembers)
  def transform(designDB: DB)(using MemberGetSet, CompilerOptions): DB =
    val patchList = designDB.membersNoGlobals.flatMap {
      // all domains are removed and their members referencing them need to point to the owner design
      case domain: DomainBlock =>
        Some(
          domain -> Patch.Replace(domain.getOwnerDesign, Patch.Replace.Config.ChangeRefAndRemove)
        )
      // named members owned by domains could need to change their name depending on the flattening mode
      // of its domain owner chain
      case member: DFMember.Named if !member.isAnonymous =>
        member.getOwner match
          case domain: DomainBlock =>
            var currentDomain: DomainBlock = domain
            var currentName = member.getName
            var inDomain = true
            // looping through domain composition until reaching a non-domain and applying the name flattening
            while (inDomain)
              currentDomain.flattenMode match
                case FlattenMode.FlattenTransparent => // no change
                case FlattenMode.FlattenPrefix(sep) =>
                  currentName = s"${currentDomain.getName}$sep$currentName"
                case FlattenMode.FlattenSuffix(sep) =>
                  currentName = s"${currentName}$sep${currentDomain.getName}"
              currentDomain.getOwner match
                case domain: DomainBlock => currentDomain = domain
                case _                   => inDomain = false
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
    designDB.patch(patchList)
  end transform
end DropDomains

extension [T: HasDB](t: T)
  def dropDomains(using CompilerOptions): DB =
    StageRunner.run(DropDomains)(t.db)
