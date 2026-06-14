package dfhdl.compiler.stages

import dfhdl.compiler.analysis.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.patching.*
import dfhdl.compiler.ir.DFDesignBlock.InstMode
import dfhdl.internals.*
import dfhdl.options.CompilerOptions
import scala.collection.mutable
import scala.collection.immutable.ListMap

case object DropDesignDefs extends GlobalStage:
  def dependencies: List[Stage] = List()
  def nullifies: Set[Stage] = Set(DFHDLUniqueNames, DropLocalDcls, DropUnreferencedAnons)
  def transformGlobal(designDB: DB)(using
      co: CompilerOptions,
      refGen: RefGen
  ): DB =
    // Patches bucketed by the sub-DB they target (keyed by `designBlock.ownerRef`).
    // The instance rename lands in the instance's parent sub-DB; the def-design
    // conversion (instMode -> Normal, redundant-ident removal, output-port move)
    // lands in the def design's own sub-DB. `newToOld` canonicalizes the parents'
    // cross-sub-DB `designRef`s onto the now-Normal block.
    val patchesByKey =
      mutable.LinkedHashMap.empty[DFOwner.Ref, mutable.ListBuffer[(DFMember, Patch)]]
    def addPatch(key: DFOwner.Ref, patch: (DFMember, Patch)): Unit =
      patchesByKey.getOrElseUpdate(key, mutable.ListBuffer.empty) += patch
    // For each def design (computed once), the name prefix derived from its output
    // port's `suggestName`, reused to name every anonymous instance of it.
    val suggestPrefixByDef = mutable.Map.empty[DFOwner.Ref, String]

    designDB.subDBs.foreach { (parentKey, parentSubDB) =>
      parentSubDB.members.foreach {
        // only going after design definition instances
        case designInst: DFDesignInst =>
          designInst.getDesignBlock(using parentSubDB.getSet) match
            case design @ DFDesignBlock(
                  domainType = DomainType.DF,
                  instMode = InstMode.Def
                ) =>
              val defKey = design.ownerRef
              // On the first instance of a given def design, stage its conversion
              // patches (in the def design's own sub-DB) and cache the output-port-
              // derived name prefix reused by every anonymous instance.
              if (!suggestPrefixByDef.contains(defKey))
                val prefix = designDB.subDBs(defKey).atGetSet {
                  val members = design.members(MemberView.Folded)
                  var outPortOpt: Option[DFVal.Dcl] = None
                  // we remove redundant ident that is wrapped around the return value
                  val identRemovePatch = members.view.reverse.collectFirst {
                    case DFNet.Connection(port @ DclOut(), ident @ Ident(retVal), _) =>
                      outPortOpt = Some(port)
                      ident -> Patch.Replace(retVal, Patch.Replace.Config.FullReplacement)
                  }.toList
                  // we need to move the output port to the end of the inputs, to
                  // prevent malformed ordering in future stages when referencing
                  // the output port.
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
                  val designPatch = design -> Patch.Replace(
                    design.copy(instMode = InstMode.Normal),
                    Patch.Replace.Config.FullReplacement
                  )
                  addPatch(defKey, designPatch)
                  identRemovePatch.foreach(addPatch(defKey, _))
                  outPortPatch.foreach(addPatch(defKey, _))
                  outPortOpt.flatMap(_.suggestName.map(x => x + "_")).getOrElse("")
                }
                suggestPrefixByDef(defKey) = prefix
              end if
              // design definition instances may be anonymous, so we name them using
              // the output port's `suggestName` (see above) plus the design class name.
              val renamedInst = parentSubDB.atGetSet {
                val updatedName =
                  if (designInst.isAnonymous)
                    suggestPrefixByDef(defKey) + s"${design.dclName}_inst"
                  else designInst.getName
                designInst.setName(updatedName)
              }
              addPatch(
                parentKey,
                designInst -> Patch.Replace(renamedInst, Patch.Replace.Config.FullReplacement)
              )
            case _ =>
        case _ =>
      }
    }

    if (patchesByKey.isEmpty) designDB
    else
      val newSubDBs = ListMap.from(
        designDB.subDBs.iterator.map { (key, subDB) =>
          patchesByKey.get(key) match
            case Some(patches) => key -> subDB.patch(patches.toList)
            case None          => key -> subDB
        }
      )
      designDB.update(subDBs = newSubDBs)
  end transformGlobal
end DropDesignDefs

//turns design definitions into normal designs, and set their instance names
//if non exist
extension [T: HasDB](t: T)
  def dropDesignDefs(using CompilerOptions): DB =
    StageRunner.run(DropDesignDefs)(t.db)
