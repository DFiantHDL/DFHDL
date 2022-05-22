package dfhdl.core
import dfhdl.internals.*
import dfhdl.compiler.ir

private abstract class Container(using DFC) extends OnCreateEvents, HasDFC:
  // TODO: revisit this. Maybe has something to do with errors and preventing repeated
  // messages. Need to check and document properly or simplify to `= summon[DFC]`
  final val dfc: DFC =
    val ret = summon[DFC]
    this match
      case _: Design => ret
      case _         => ret.copy()
  private[core] type TScope <: DFC.Scope
  private[core] type TDomain <: DFC.Domain
  private[core] lazy val __domainType: ir.DomainType
  private[core] lazy val owner: DFOwnerAny
  dfc.enterOwner(owner)
  private[dfhdl] def skipChecks: Boolean = false

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
