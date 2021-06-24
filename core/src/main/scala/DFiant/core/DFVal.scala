package DFiant.core
import DFiant.compiler.ir
import DFiant.internals.*

opaque type DFVal[+T <: DFType, +M <: DFVal.Modifier] = ir.DFVal
type DFValOf[+T <: DFType] = DFVal[T, DFVal.Modifier.VAL]
type DFVarOf[+T <: DFType] = DFVal[T, DFVal.Modifier.Assignable]
type DFPortOf[+T <: DFType] = DFVal[T, DFVal.Modifier.Port]

extension (dfVal: ir.DFVal)
  def asValOf[T <: DFType]: DFValOf[T] = dfVal
  def asVarOf[T <: DFType]: DFVarOf[T] = dfVal
  def asPortOf[T <: DFType]: DFPortOf[T] = dfVal

object DFVal:
  export ir.DFVal.Modifier

  extension [T <: DFType, M <: Modifier](dfVal: DFVal[T, M])
    def dfType: T = dfVal.asIR.dfType.asInstanceOf[T]
    def width(using w: Width[T]): Inlined.Int[w.Out] =
      Inlined.Int.forced[w.Out](dfVal.asIR.dfType.width)
    def asIR: ir.DFVal = dfVal

  object Const:
    def apply[T <: DFType](token: DFToken)(using DFC): DFValOf[T] =
      ir.DFVal
        .Const(token.asIR, dfc.owner.ref, dfc.getMeta, ir.DFTags.empty)
        .addMember

  object Dcl:
    def apply[T <: DFType, M <: Modifier](dfType: T, modifier: M)(using
        DFC
    ): DFValNI[T, M] =
      ir.DFVal
        .Dcl(
          dfType.asIR,
          modifier,
          None,
          dfc.owner.ref,
          dfc.getMeta,
          ir.DFTags.empty
        )
        .addMember
    def apply[T <: DFType, M <: Modifier](
        dfVal: DFValNI[T, M],
        init: Seq[DFToken]
    )(using DFC): DFVal[T, M] =
      dfVal
        .asInstanceOf[ir.DFVal.Dcl]
        .copy(externalInit = Some(init.map(_.asIR)))
end DFVal

opaque type DFValNI[+T <: DFType, +M <: DFVal.Modifier] <: DFVal[T, M] =
  DFVal[T, M]
object DFValNI:
  extension [T <: DFType, M <: DFVal.Modifier](dcl: DFValNI[T, M])
    inline def init(inline token: Any*)(using DFC): DFVal[T, M] =
      DFVal.Dcl(dcl, ???)
