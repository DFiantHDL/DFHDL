package dfhdl.compiler.stages

import dfhdl.compiler.analysis.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.patching.*
import dfhdl.options.CompilerOptions
import dfhdl.internals.*
import DFVal.Func.Op as FuncOp
import scala.collection.mutable
//format: off
/** This stage transforms:
  *   - wait statements into while loops with a cycle wait. For example:
  *     waitWhile(cond) -> while(cond) 1.cy.wait
  *   - rising/falling edge operations into reg alias detection operations. For example:
  *     i.rising -> !i.reg(1, init = 1) && i
  *     i.falling -> i.reg(1, init = 0) && !i
  */
//format: on
case object SimplifyRTOps extends Stage:
  def dependencies: List[Stage] = List()
  def nullifies: Set[Stage] = Set(DropUnreferencedAnons)

  def transform(designDB: DB)(using MemberGetSet, CompilerOptions): DB =
    extension (dfVal: DFVal)
      def isAnonReferencedByWait: Boolean = dfVal.isAnonymous && dfVal.originMembers.view.exists {
        case _: Wait => true
        case _       => false
      }
    val patchList = designDB.members.view.collect {
      case trigger @ DFVal.Func(
            _,
            op @ (FuncOp.rising | FuncOp.falling),
            List(DFRef(arg)),
            _,
            _,
            _
          ) if trigger.isRTDomain && !trigger.isAnonReferencedByWait =>
        val dsn = new MetaDesign(
          trigger,
          Patch.Add.Config.ReplaceWithLast(Patch.Replace.Config.FullReplacement),
          dfhdl.core.DomainType.RT(dfhdl.core.RTDomainCfg.Derived)
        ):
          val argFE = arg.asValOf[dfhdl.core.DFBit]
          op match
            case FuncOp.rising =>
              (!argFE.reg(1, init = 1)).&&(argFE)(using dfc.setMeta(trigger.meta))
            case FuncOp.falling =>
              argFE.reg(1, init = 0).&&(!argFE)(using dfc.setMeta(trigger.meta))
        Some(dsn.patch)

      case waitMember @ Wait(DFRef(trigger @ DFBoolOrBit.Val(_)), _, _, _)
          if waitMember.isRTDomain =>
        // Create a while loop with a cycle wait
        val dsn = new MetaDesign(
          waitMember,
          Patch.Add.Config.ReplaceWithLast(Patch.Replace.Config.FullReplacement),
          dfhdl.core.DomainType.RT(dfhdl.core.RTDomainCfg.Derived)
        ):
          // If the trigger is a rising or falling edge, we need to negate it
          val fixedTrigger = trigger match
            case DFVal.Func(_, op @ (FuncOp.rising | FuncOp.falling), List(DFRef(arg)), _, _, _) =>
              if (trigger.isAnonReferencedByWait)
                val argFE = arg.asValOf[dfhdl.core.DFBit]
                op match
                  case FuncOp.rising =>
                    argFE.reg(1, init = 1).||(!argFE)(using dfc.setMeta(trigger.meta))
                  case FuncOp.falling =>
                    (!argFE.reg(1, init = 0)).||(argFE)(using dfc.setMeta(trigger.meta))
              else !(trigger.asValOf[dfhdl.core.DFBool])
            case _ =>
              trigger.asValOf[dfhdl.core.DFBoolOrBit]
          val whileBlock =
            dfhdl.core.DFWhile.Block(fixedTrigger)(using dfc.setMeta(waitMember.meta))
          dfc.enterOwner(whileBlock)
          1.cy.wait
          dfc.exitOwner()
        Some(dsn.patch)
    }.flatten.toList

    designDB.patch(patchList)
  end transform
end SimplifyRTOps

extension [T: HasDB](t: T)
  def simplifyRTOps(using CompilerOptions): DB =
    StageRunner.run(SimplifyRTOps)(t.db)
