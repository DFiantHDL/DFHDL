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
  *   - non-zero/one cycle wait statements to while loops with a counter. For example (under 50Mhz clock):
  *     50000000.cy.wait -> while(waitCnt != 50000000) 1.cy.wait
  */
//format: on
case object SimplifyRTOps extends Stage:
  def dependencies: List[Stage] = List(DropTimedRTWaits, DFHDLUniqueNames)
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
          ) if trigger.isInRTDomain && !trigger.isAnonReferencedByWait =>
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

      case waitMember @ Wait(triggerRef = DFRef(trigger @ DFBoolOrBit.Val(_)))
          if waitMember.isInRTDomain =>
        // Create a while loop with a cycle wait
        val dsn = new MetaDesign(
          waitMember,
          Patch.Add.Config.ReplaceWithLast(Patch.Replace.Config.FullReplacement),
          dfhdl.core.DomainType.RT(dfhdl.core.RTDomainCfg.Derived)
        ):
          // If the trigger is a rising or falling edge, we need to negate it
          val fixedTrigger = trigger match
            case DFVal.Func(op = op @ (FuncOp.rising | FuncOp.falling), args = List(DFRef(arg))) =>
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
      // replace wait statements with time durations with cycles
      case waitMember @ Wait(triggerRef = DFRef(cyclesVal @ DFDecimal.Val(DFUInt(_)))) =>
        val replaceWithWhile = cyclesVal match
          // if the number of cycles is 1, then there is no need to create a loop.
          // if the number of cycles is 0, then its an indicator for while loops that are combinational
          case DFVal.Const(data = Some(value: BigInt)) if value <= 1 => false
          case _                                                     => true
        if (replaceWithWhile)
          val dsn = new MetaDesign(
            waitMember,
            Patch.Add.Config.ReplaceWithLast(Patch.Replace.Config.FullReplacement),
            dfhdl.core.DomainType.RT(dfhdl.core.RTDomainCfg.Derived)
          ):
            val iterType = cyclesVal.asValOf[UInt[Int]].dfType
            // TODO: unclear why we cannot directly use 0 and 1 here, but there is indication that
            // the bug originates from a conversion from UInt[1] to the iterType. It could be that the
            // methods under core.DFVal.Alias are not working as intended because of meta-programming.
            val initZero = dfhdl.core.DFVal.Const(iterType, Some(BigInt(0)))
            val waitCnt =
              iterType.<>(VAR.REG).initForced(List(initZero))(using dfc.setName("waitCnt"))
            // the upper bound for the while loop count
            val upperBound = cyclesVal match
              // the upper bound is reduced to a simpler form when the number of cycles is a constant anonymous value
              case DFVal.Const(data = Some(value: BigInt)) if cyclesVal.isAnonymous =>
                dfhdl.core.DFVal.Const(iterType, Some(value - 1))
              // the upper bound is an expression when the number of cycles is a variable
              case _ =>
                cyclesVal.asValOf[UInt[Int]] - dfhdl.core.DFVal.Const(iterType, Some(BigInt(1)))
            val whileBlock =
              dfhdl.core.DFWhile.Block(waitCnt != upperBound)(using dfc.setMeta(waitMember.meta))
            dfc.enterOwner(whileBlock)
            waitCnt.din := waitCnt + dfhdl.core.DFVal.Const(iterType, Some(BigInt(1)))
            1.cy.wait
            dfc.exitOwner()
          Some(dsn.patch)
        else None
        end if
    }.flatten.toList

    designDB.patch(patchList)
  end transform
end SimplifyRTOps

extension [T: HasDB](t: T)
  def simplifyRTOps(using CompilerOptions): DB =
    StageRunner.run(SimplifyRTOps)(t.db)
