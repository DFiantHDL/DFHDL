package dfhdl.core
import dfhdl.compiler.ir
import dfhdl.internals.*
import scala.annotation.implicitNotFound

type StepBlock = DFOwner[ir.StepBlock]
object StepBlock:
  def apply(using dfc: DFC): StepBlock =
    val step: ir.StepBlock = ir.StepBlock(
      ir.DFMember.Empty.ref,
      dfc.getMeta,
      dfc.tags
    )
    step.asFE
end StepBlock

sealed trait Step
object Step extends Step:
  object Ops:
    def ThisStep(using dfc: DFC): Step =
      ir.Goto(
        ir.Goto.ThisStep.refTW[ir.Goto],
        dfc.owner.ref,
        dfc.getMeta,
        dfc.tags
      ).addMember
      Step
    def NextStep(using dfc: DFC): Step =
      ir.Goto(
        ir.Goto.NextStep.refTW[ir.Goto],
        dfc.owner.ref,
        dfc.getMeta,
        dfc.tags
      ).addMember
      Step
    def FirstStep(using dfc: DFC): Step =
      ir.Goto(
        ir.Goto.FirstStep.refTW[ir.Goto],
        dfc.owner.ref,
        dfc.getMeta,
        dfc.tags
      ).addMember
      Step
  end Ops
  // this is called by the compiler plugin to register all steps (for each step block) in the
  def pluginRegisterStep(stepMeta: ir.Meta)(using
      dfc: DFC,
      scope: DFC.Scope.Process
  ): Unit =
    val step = StepBlock(using dfc.setMeta(stepMeta))
    scope.stepCache += (stepMeta.name -> step.asIR)

  // this is called by the compiler plugin and replaces the step's `def`. this will add the
  // step to the context, update its reference to point to the proper owner, and finally run
  // the step's body with the step as its owner.
  def pluginAddStep(stepName: String)(
      run: => Unit
  )(using dfc: DFC, scope: DFC.Scope.Process): Unit =
    val stepIR = scope.stepCache(stepName)
    stepIR.addMember
    dfc.mutableDB.newRefFor(stepIR.ownerRef, dfc.owner.asIR)
    dfc.enterOwner(stepIR.asFE)
    run
    dfc.exitOwner()

  // this is called by the compiler plugin and replaces the step's onEntry/onExit `def` with
  // a step block that has the name "onEntry"/"onExit" which is special-cases and not treated
  // as a normal step, but just a container for the onEntry/onExit code.
  def pluginOnEntryExit(meta: ir.Meta)(
      run: => Unit
  )(using dfc: DFC): Unit =
    val onEntryExit = ir.StepBlock(
      dfc.owner.ref,
      meta,
      dfc.tags
    ).addMember
    dfc.enterOwner(onEntryExit.asFE)
    run
    dfc.exitOwner()

  // this is called by the compiler plugin and replaces references (calls) to the step `def`.
  // for the process this is considered as a goto statement.
  def pluginGotoStep(nextStepName: String)(using dfc: DFC, scope: DFC.Scope.Process): Unit =
    import dfc.getSet
    val nextStep = scope.stepCache(nextStepName)
    val member: ir.Goto = ir.Goto(
      nextStep.refTW[ir.Goto],
      dfc.owner.ref,
      dfc.getMeta,
      dfc.tags
    )
    member.addMember
end Step
