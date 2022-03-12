package DFiant.core
import DFiant.internals.*
import DFiant.compiler.ir

private abstract class Container(using DFC) extends OnCreateEvents, HasDFC:
  final val dfc: DFC =
    val ret = summon[DFC]
    this match
      case _: Design => ret
      case _         => ret.copy()
  private[core] type TKind <: Container.Kind
  private[core] type TDomain <: ir.DomainType
  private[core] lazy val domainType: TDomain
  final protected given TDomain = domainType
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
