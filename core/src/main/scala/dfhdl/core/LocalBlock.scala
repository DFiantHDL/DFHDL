package dfhdl.core
import dfhdl.internals.*
import dfhdl.compiler.ir

object LocalBlock:
  type Block = DFOwner[ir.LocalBlock]
  object Block:
    def apply(using DFC): Block =
      ir.LocalBlock(
        dfc.owner.ref,
        dfc.getMeta,
        dfc.tags
      ).addMember.asFE
  end Block

  object Ops:
    // the actual local-block construction + body execution, kept out of the inline method
    private def locallyDFHDL[A](body: => A)(using dfc: DFC): A =
      val owner = Block.apply(using dfc)
      dfc.enterOwner(owner)
      val ret = body
      dfc.exitOwner()
      ret
    // `locally` is overloaded just like `println`/`print` in TextOut.scala: inside a process
    // scope it constructs a DFHDL LocalBlock; otherwise it falls back to scala.Predef.locally.
    transparent inline def locally[A](inline body: => A): A =
      compiletime.summonFrom {
        case given DFC.Scope.Process =>
          locallyDFHDL(body)(using compiletime.summonInline[DFC])
        case _ => scala.Predef.locally(body)
      }
  end Ops
end LocalBlock
