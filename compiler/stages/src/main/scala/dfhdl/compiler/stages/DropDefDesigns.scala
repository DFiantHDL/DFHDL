package dfhdl.compiler.stages

import dfhdl.compiler.analysis.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.patching.*
import dfhdl.compiler.ir.DFDesignBlock.InstMode
case object DropDefDesigns extends Stage:
  def dependencies: List[Stage] = List()
  def nullifies: Set[Stage] = Set(DFHDLUniqueNames, DropLocalDcls)
  def transform(designDB: DB)(using MemberGetSet): DB =
    val patchList = designDB.designMemberList.collect {
      // only going after definition designs
      case (d @ DFDesignBlock(DomainType.DF, _, InstMode.Def, _, _, _), members) =>
        // definitions designs may be anonymous, so we name them
        val updatedName =
          if (d.isAnonymous)
            // the output port is connected and used in the function and from that
            // we know the target name using `suggestName`
            val outPort = members.view.reverse.collectFirst {
              case port @ PortOfDefDesign(DFVal.Modifier.OUT, _) => port
            }.get
            outPort.suggestName.map(x => x + "_").getOrElse("") + s"${d.dclName}_inst"
          else d.getName
        d -> Patch.Replace(
          d.copy(instMode = InstMode.Normal).setName(updatedName),
          Patch.Replace.Config.FullReplacement
        )
    }
    designDB.patch(patchList)
  end transform
end DropDefDesigns

//turns definitions designs into normal designs, and set their instance names
//if non exist
extension [T: HasDB](t: T)
  def dropDefDesigns: DB =
    StageRunner.run(DropDefDesigns)(t.db)(using dfhdl.options.CompilerOptions.default)
