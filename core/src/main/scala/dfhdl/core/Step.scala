package dfhdl.core
import dfhdl.compiler.ir
import dfhdl.internals.*
import scala.annotation.implicitNotFound

type Step = DFOwner[ir.StepBlock]
object Step:
  object Block:
    def apply(using dfc: DFC): Step =
      val step: ir.StepBlock = ir.StepBlock(
        ir.DFMember.Empty.ref,
        dfc.getMeta,
        dfc.tags
      )
      step.asFE
  end Block
  // this is called by the compiler plugin to register all steps (for each step block) in the
  // beginning of the process. we only construct the step block IR and without its actual
  // owner reference.
  def pluginRegisterStep(stepMeta: ir.Meta, onEntry: => Unit, onExit: => Unit)(using
      dfc: DFC,
      scope: DFC.Scope.Process
  ): Unit =
    val step = Block(using dfc.setMeta(stepMeta))
    scope.stepCache += (stepMeta.name -> (step.asIR, () => onEntry, () => onExit))

  // this is called by the compiler plugin and replaces the step's `def`. this will add the
  // step to the context, update its reference to point to the proper owner, and finally run
  // the step's body with the step as its owner.
  def pluginAddStep(stepName: String)(
      run: => Unit
  )(using dfc: DFC, scope: DFC.Scope.Process): Unit =
    val stepIR = scope.stepCache(stepName).stepBlock
    stepIR.addMember
    dfc.mutableDB.newRefFor(stepIR.ownerRef, dfc.owner.asIR)
    dfc.enterOwner(stepIR.asFE)
    run
    dfc.exitOwner()

  // this is called by the compiler plugin and replaces references (calls) to the step `def`.
  // for the process this is considered as a goto statement.
  def pluginGotoStep(nextStepName: String)(using dfc: DFC, scope: DFC.Scope.Process): Unit =
    import dfc.getSet
    val currentStepName = dfc.owner.asIR.getThisOrOwnerStepBlock.getName
    val nextStep = scope.stepCache(nextStepName)
    if (currentStepName != nextStepName)
      scope.stepCache(currentStepName).onExit()
      nextStep.onEntry()
    val member: ir.Goto = ir.Goto(
      nextStep.stepBlock.refTW[ir.Goto],
      dfc.owner.ref,
      dfc.getMeta,
      dfc.tags
    )
    member.addMember
end Step
