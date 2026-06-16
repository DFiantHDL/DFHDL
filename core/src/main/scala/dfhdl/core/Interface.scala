package dfhdl.core
import dfhdl.internals.*
import dfhdl.compiler.ir
import ir.DFDesignBlock.InstMode

import scala.annotation.Annotation

// A basic interface declaration: structurally an "empty design" that carries
// ports and views but no behavioral statements (no processes/connections/
// assignments). It elaborates to a `DFDesignBlock(InstMode.Interface)` and runs
// under `DFC.Scope.Interface`, which is used to reject illegal constructs inside
// an interface body. The lifecycle mirrors `Design` but drops the top-level /
// device-top / resource and top-check machinery (never applies to an interface).
//
// An interface is purely structural, so it has no behavioral (DF/RT/ED) domain
// semantics of its own. There is a single, domain-neutral `Interface` based on
// the ED domain under the hood: ED is terminal in the lowering pipeline
// (DF -> RT -> ED), so an ED interface is never transformed by ToRT/ToED and
// never has clk/rst injected, and the same `Interface` is reusable inside a
// design of any domain.
abstract class Interface
    extends DomainContainer(DomainType.ED),
      HasClsMetaArgs,
      HasConstParams:
  private[core] type TScope = DFC.Scope.Interface
  private[core] type TOwner = Design.Block
  final protected given TScope = DFC.Scope.Interface
  private[core] def mkInstMode: InstMode = InstMode.Interface
  private[dfhdl] def initOwner: TOwner =
    Design.Block(__domainType, InstMode.Interface)(using dfc.anonymize)
  final protected def setClsNamePos(
      name: String,
      position: Position,
      docOpt: Option[String],
      annotations: List[Annotation]
  ): Unit =
    import dfc.getSet
    val designBlock = containedOwner.asIR
    getSet.replace(designBlock)(
      designBlock.copy(
        meta = r__For_Plugin.metaGen(Some(name), position, docOpt, annotations),
        instMode = mkInstMode
      )
    )
  end setClsNamePos
  private var hasStartedLate: Boolean = false
  final override def onCreateStartLate: Unit =
    hasStartedLate = true
    val paramEntries = Design.Inst.collectParamEntries
    val endedInterface = containedOwner.asIR
    dfc.exitOwner()
    Design.Inst(endedInterface, paramEntries)
    dfc.enterLate()
  final override def onCreateEnd(thisOwner: Option[This]): Unit =
    if (hasStartedLate) dfc.exitLate()
    else dfc.exitOwner()
end Interface
