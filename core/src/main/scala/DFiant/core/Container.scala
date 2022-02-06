package DFiant.core
import DFiant.internals.*
import DFiant.compiler.ir

export ir.Domain
private abstract class Container(using DFC) extends OnCreateEvents, LateConstruction, HasDFC:
  final val dfc: DFC = summon[DFC]
  private[core] type TDmn <: Domain
  private[core] lazy val owner: DFOwnerAny
  dfc.enterOwner(owner)

  final override def onCreateEnd: Unit =
    dfc.exitOwner()
