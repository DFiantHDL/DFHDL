package dfhdl.platforms.resources
import dfhdl.platforms.resources.Resource.CanConnect
import dfhdl.internals.*
import dfhdl.core.*
import dfhdl.compiler.ir.constraints.SigConstraint
import dfhdl.compiler.ir.annotation.HWAnnotation
trait IO extends Resource:
  @dfhdl.internals.metaContextIgnore
  override protected[dfhdl] def connect(that: DFValAny)(using dfc: DFC): Unit =
    import dfc.getSet
    import dfhdl.compiler.analysis.DclPort
    that.asIR.departialDcl match
      case Some(dcl @ DclPort(), range) if dcl.getOwnerDesign.isTop =>
        val newSigConstraints =
          if (range.length != dcl.width) this.allSigConstraints.flatMap { cs =>
            for (i <- range) yield cs.updateBitIdx(i)
          }
          else this.allSigConstraints
        val (existingSigConstraints, otherAnnotations) = dcl.meta.annotations.partition {
          case cs: SigConstraint => true
          case _                 => false
        }.asInstanceOf[(List[SigConstraint], List[HWAnnotation])]
        val updatedSigConstraints =
          (existingSigConstraints ++ newSigConstraints).merge.consolidate(dcl.width)
        val updatedAnnotations = updatedSigConstraints ++ otherAnnotations
        dcl.setMeta(m => m.copy(annotations = updatedAnnotations))
      case _ =>
        throw new IllegalArgumentException(
          "Cannot apply resource constraints to a non-top-level port value."
        )
    end match
  end connect
end IO
object IO:
  given [T <: IO, R <: IO]: CanConnect[T, R] = (resource1: T, resource2: R) =>
    resource1.connectFrom(resource2)
    resource2.connectFrom(resource1)
  given [R <: IO, V <: DFValOf[DFBoolOrBit]](using
      dfc: DFC
  ): CanConnect[R, V] =
    (resource: R, dfVal: V) => resource.connect(dfVal)
