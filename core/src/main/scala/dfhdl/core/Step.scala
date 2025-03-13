package dfhdl.core
import dfhdl.compiler.ir
import dfhdl.internals.*
import scala.annotation.implicitNotFound

final class Step(val irValue: ir.Step | DFError) extends AnyVal with DFMember[ir.Step]
object Step:
  extension (net: ir.Step) def asFE: Step = new Step(net)
  object Ops:
    def step(using
        dfc: DFC,
        @implicitNotFound("`step` can only be used inside a process.") scope: DFC.Scope.Process,
        @implicitNotFound(
          "`step` can only be used under register-transfer (RT) domains."
        ) check: DomainType.RT
    ): Step =
      scope.stepCache.updateWith(dfc.getMeta) {
        case Some(step) => Some(step)
        case None       =>
          // the step's owner could be currently unknown because of forward reference
          // by a goto statement. during the compiler plugin call of `addStep` we
          // update the step's owner reference to point to the proper owner where the
          // step is declared.
          val step: ir.Step = ir.Step(
            ir.DFMember.Empty.ref,
            dfc.getMeta,
            dfc.tags
          )
          Some(step)
      }.get.asFE
    extension (step: Step)
      def goto(using
          dfc: DFC,
          @implicitNotFound("`goto` can only be used inside a process.") scope: DFC.Scope.Process,
          @implicitNotFound(
            "`goto` can only be used under register-transfer (RT) domains."
          ) check: DomainType.RT
      ): Unit =
        val member: ir.Goto = ir.Goto(
          step.asIR.refTW[ir.Goto],
          dfc.owner.ref,
          dfc.getMeta,
          dfc.tags
        )
        member.addMember
    end extension
  end Ops
end Step
