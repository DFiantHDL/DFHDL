package DFiant.core
import DFiant.internals.*
import DFiant.compiler.ir

private abstract class Container(using DFC) extends OnCreateEvents, LateConstruction, HasDFC:
  final val dfc: DFC = summon[DFC]
  private[core] type TKind <: Container.Kind
  private[core] type TDomain <: Container.Domain
  private[core] lazy val owner: DFOwnerAny
  dfc.enterOwner(owner)

  final override def onCreateEnd: Unit =
    dfc.exitOwner()

object Container:
  sealed trait Kind
  object Kind:
    sealed trait Design extends Kind
    object Design extends Design
    sealed trait Domain extends Kind
    object Domain extends Domain
    sealed trait Process extends Kind
    object Process extends Process
    sealed trait Interface extends Kind
    object Interface extends Interface
  sealed trait Domain
  object Domain:
    sealed trait DF extends Domain
    object DF extends DF
    class HLRT extends Domain
    class LLRT extends Domain
end Container
