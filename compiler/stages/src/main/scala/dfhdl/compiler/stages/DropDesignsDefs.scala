package dfhdl.compiler.stages

import dfhdl.compiler.analysis.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.patching.*
import dfhdl.compiler.ir.DFDesignBlock.InstMode
import dfhdl.options.CompilerOptions
case object DropDesignDefs extends Stage:
  def dependencies: List[Stage] = List()
  def nullifies: Set[Stage] = Set(DFHDLUniqueNames, DropLocalDcls, DropUnreferencedAnons)
  def transform(designDB: DB)(using MemberGetSet, CompilerOptions): DB =
    val patchList = designDB.designMemberList.flatMap {
      // only going after design definitions
      case (design @ DFDesignBlock(domainType = DomainType.DF, instMode = InstMode.Def), members) =>
        var outPortOpt: Option[DFVal.Dcl] = None
        // we remove redundant ident that is wrapped around the return value
        val identRemovePatch = members.view.reverse.collectFirst {
          case DFNet.Connection(port @ DclOut(), ident @ Ident(retVal), _) =>
            outPortOpt = Some(port)
            ident -> Patch.Replace(retVal, Patch.Replace.Config.FullReplacement)
        }.toList
        val updatedName =
          // design definitions may be anonymous, so we name them
          if (design.isAnonymous)
            // the output port is connected and used in the function and from that
            // we know the target name using `suggestName`
            outPortOpt
              .flatMap(_.suggestName.map(x => x + "_"))
              .getOrElse("") + s"${design.dclName}_inst"
          else design.getName

        // we need to move the output port to the end of the inputs, to prevent
        // malformed ordering in future stages when referencing the output port.
        val lastInputOpt = members.dropWhile {
          case DclIn() => false
          case _       => true
        }.takeWhile {
          case DclIn() => true
          case _       => false
        }.lastOption

        val outPortPatch = outPortOpt.map { o =>
          lastInputOpt match
            case Some(posMember) => posMember -> Patch.Move(o, Patch.Move.Config.After)
            // if there are no inputs, we move the output port to the beginning
            case None => members.head -> Patch.Move(o, Patch.Move.Config.Before)
        }
        design -> Patch.Replace(
          design.copy(instMode = InstMode.Normal).setName(updatedName),
          Patch.Replace.Config.FullReplacement
        ) :: identRemovePatch ++ outPortPatch
      case _ => None
    }
    designDB.patch(patchList)
  end transform
end DropDesignDefs

//turns design definitions into normal designs, and set their instance names
//if non exist
extension [T: HasDB](t: T)
  def dropDesignDefs(using CompilerOptions): DB =
    StageRunner.run(DropDesignDefs)(t.db)
