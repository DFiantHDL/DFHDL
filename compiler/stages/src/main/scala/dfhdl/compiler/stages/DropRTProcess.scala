package dfhdl.compiler.stages

import dfhdl.compiler.analysis.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.patching.*
import dfhdl.options.CompilerOptions
import dfhdl.internals.*
import DFVal.Func.Op as FuncOp
import scala.collection.mutable
import scala.collection.immutable.ListMap
import dfhdl.core.DFType.asFE
import dfhdl.core.{DFValAny, DFOwnerAny, asValAny, DFC}
//format: off
/** This stage drops RT processes by creating an explicit enumerated state machines
  */
//format: on
case object DropRTProcess extends Stage:
  def dependencies: List[Stage] = List() // DropRTWaits
  def nullifies: Set[Stage] = Set(DFHDLUniqueNames)

  def transform(designDB: DB)(using MemberGetSet, CompilerOptions): DB =
    val patchList = designDB.members.view.collect {
      case pb: ProcessBlock if pb.isInRTDomain =>
        val stateBlocks = pb.members(MemberView.Folded).collect { case sb: StepBlock =>
          sb
        }
        val gotos = pb.members(MemberView.Flattened).collect { case g: Goto => g }
        val pbPatch = pb -> Patch.Replace(pb.getOwner, Patch.Replace.Config.ChangeRefAndRemove)
        if (stateBlocks.isEmpty) List(pbPatch)
        else
          val enumName = if (pb.isAnonymous) s"State" else s"${pb.getName}_State"
          val stateRegName = if (pb.isAnonymous) s"state" else s"${pb.getName}_state"
          val entries = ListMap.from(stateBlocks.view.zipWithIndex.map { case (sb, idx) =>
            sb.getName -> BigInt(idx)
          })
          val stateEnumIR = DFEnum(enumName, stateBlocks.length.bitsWidth(false), entries)
          type StateEnum = dfhdl.core.DFEnum[dfhdl.core.DFEncoding]
          val stateEnumFE = stateEnumIR.asFE[StateEnum]
          def enumEntry(value: BigInt)(using DFC) = dfhdl.core.DFVal.Const(stateEnumFE, Some(value))
          val dsn = new MetaDesign(
            pb,
            Patch.Add.Config.Before,
            dfhdl.core.DomainType.RT(dfhdl.core.RTDomainCfg.Derived)
          ):
            val stateInit = dfhdl.core.DFVal.Const(stateEnumFE, Some(entries.head._2))
            val patterns = entries.map { case (_, value) =>
              dfhdl.core.DFMatch.Pattern.Singleton(enumEntry(value))
            }
            val stateReg = stateEnumFE.<>(VAR.REG).init(stateInit)(using dfc.setName(stateRegName))
            val header = dfhdl.core.DFMatch.Header(dfhdl.core.DFUnit, stateReg).asIR
          val stateReg = dsn.stateReg
          var prevBlockOrHeader: DFOwnerAny | DFValAny = dsn.header.asValAny
          val caseDsns = stateBlocks.lazyZip(dsn.patterns).map { (sb, pattern) =>
            new MetaDesign(
              sb,
              Patch.Add.Config.ReplaceWithLast(Patch.Replace.Config.ChangeRefAndRemove),
              dfhdl.core.DomainType.RT(dfhdl.core.RTDomainCfg.Derived)
            ):
              val block = dfhdl.core.DFMatch.Block(pattern, None, prevBlockOrHeader)(using
                dfc.setMeta(sb.meta)
              )
              prevBlockOrHeader = block
          }
          val gotoDsns = gotos.map { g =>
            new MetaDesign(
              g,
              Patch.Add.Config.ReplaceWithLast(Patch.Replace.Config.ChangeRefAndRemove),
              dfhdl.core.DomainType.RT(dfhdl.core.RTDomainCfg.Derived)
            ):
              stateReg.din.:=(enumEntry(entries(g.stepRef.get.getName)))(using dfc.setMeta(g.meta))
          }
          Iterator(
            List(dsn.patch, pbPatch),
            caseDsns.map(_.patch),
            gotoDsns.map(_.patch)
          ).flatten
        end if
    }.flatten.toList

    designDB.patch(patchList)
  end transform
end DropRTProcess

extension [T: HasDB](t: T)
  def dropRTProcess(using CompilerOptions): DB =
    StageRunner.run(DropRTProcess)(t.db)
