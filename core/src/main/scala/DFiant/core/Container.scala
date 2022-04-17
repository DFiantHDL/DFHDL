package DFiant.core
import DFiant.internals.*
import DFiant.compiler.ir

private abstract class Container(using DFC) extends OnCreateEvents, HasDFC:
  // TODO: revisit this. Maybe has something to do with errors and preventing repeated
  // messages. Need to check and document properly or simplify to `= summon[DFC]`
  final val dfc: DFC =
    val ret = summon[DFC]
    this match
      case _: Design => ret
      case _         => ret.copy()
  private[core] type TScope <: Container.Scope
  private[core] type TDomain <: Container.Domain
  private[core] lazy val __domainType: ir.DomainType
  private[core] lazy val owner: DFOwnerAny
  dfc.enterOwner(owner)
  private[DFiant] def skipChecks: Boolean = false

  final override def onCreateEnd: Unit =
    dfc.exitOwner()
    import dfc.getSet
    // At the end of the top-level instance we check for errors
    if (owner.asIR.isTop)
      val errors = dfc.getErrors
      // If we have errors, then we print them to stderr and exit
      if (errors.nonEmpty)
        exitWithError(
          errors.collect { case basicErr: DFError.Basic => basicErr.toString }.mkString("\n\n")
        )
      if (!skipChecks)
        try dfc.mutableDB.immutable.connectionTable // this does connectivity checks
        catch
          case err: IllegalArgumentException =>
            exitWithError(err.getMessage)
          case others => throw others
    end if
  end onCreateEnd
end Container

object Container:
  sealed trait Scope
  object Scope:
    sealed trait Design extends Scope
    object Design extends Design
    given Design = Design
    sealed trait Domain extends Scope
    object Domain extends Domain
    sealed trait Process extends Scope
    object Process extends Process
    sealed trait Interface extends Scope
    object Interface extends Interface
  sealed trait Domain
  object Domain:
    sealed trait DF extends Domain
    object DF extends DF
    sealed trait RT extends Domain
    object RT extends RT
    sealed trait ED extends Domain
    object ED extends ED
end Container
