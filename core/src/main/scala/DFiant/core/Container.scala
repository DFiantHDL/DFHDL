package DFiant.core
import DFiant.internals.*
import DFiant.compiler.ir

private abstract class Container(using DFC) extends OnCreateEvents, HasDFC:
  final val dfc: DFC = summon[DFC]
  private[core] type TKind <: Container.Kind
  private[core] type TDomain <: ir.Domain
  private[core] lazy val domain: TDomain
  final protected given TDomain = domain
  private[core] lazy val owner: DFOwnerAny
  dfc.enterOwner(owner)

  final override def onCreateStartLate: Unit =
    dfc.enterLate()
  final override def onCreateEnd: Unit =
    dfc.exitOwner()
    import dfc.getSet
    //At the end of the top-level instance we check for errors
    if (owner.asIR.isTop)
      val errors = dfc.getErrors
      // If we have errors, then we print them to stderr and exit
      if (errors.nonEmpty)
        exitWithError(errors.collect{case basicErr : DFError.Basic => basicErr.toString}.mkString("\n\n"))
    end if

object Container:
  sealed trait Kind
  object Kind:
    sealed trait Design extends Kind
    object Design extends Design
    sealed trait Domain extends Kind
    object Domain extends Domain
    sealed trait Always extends Kind
    object Always extends Always
    sealed trait Interface extends Kind
    object Interface extends Interface
  sealed trait Domain
  object Domain:
    sealed trait DF extends Domain
    object DF extends DF
    class HLRT extends Domain
    class LLRT extends Domain
end Container
