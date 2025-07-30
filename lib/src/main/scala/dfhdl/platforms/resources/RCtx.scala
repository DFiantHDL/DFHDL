package dfhdl.platforms.resources
import dfhdl.internals.CTName
import dfhdl.internals.PrintType
import scala.annotation.implicitNotFound
import dfhdl.internals.CTAnnotations
import dfhdl.compiler.ir.constraints.Constraint

//Resource Context
@implicitNotFound("Missing RCtx context")
trait RCtx:
  val id: String
  val owner: ResourceOwner
  val constraints: List[Constraint]

object RCtx:
  type NonEmptyName[T] = util.NotGiven[T =:= ""]
  given RCtx(using
      ctName: CTName,
      ctAnnotations: CTAnnotations,
      owner: ResourceOwner = NoResourceOwner
  )(using NonEmptyName[ctName.Out]): RCtx = new RCtx:
    val id = ctName.value
    val owner = owner
    val constraints = ctAnnotations.annotations.collect {
      case c: dfhdl.hw.constraints.Constraint => c.asIR
    }
