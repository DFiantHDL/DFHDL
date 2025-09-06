package dfhdl.platforms.resources
import dfhdl.platforms.resources.Resource.CanConnect
import dfhdl.internals.*
import dfhdl.core.*
import dfhdl.compiler.ir.constraints.SigConstraint
import dfhdl.compiler.ir.annotation.HWAnnotation
trait IO extends Resource:
  override protected[dfhdl] def connect(that: DFValAny)(using dfc: DFC): Unit =
    import dfc.getSet
    import dfhdl.compiler.analysis.DclPort
    that.asIR.departialDcl match
      case Some(dcl @ DclPort(), range) =>
        dfc.mutableDB.ResourceOwnershipContext.connectDclResource(dcl, range, this)
      case _ =>
        throw new IllegalArgumentException(
          "Cannot connect resource to a non-port value."
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
