package dfhdl.platforms.resources
import dfhdl.internals.CTName
import dfhdl.internals.PrintType
import scala.annotation.implicitNotFound

//Resource Context
@implicitNotFound("Missing RCtx context")
trait RCtx:
  val id: String
  val owner: ResourceOwner

object RCtx:
  type NonEmptyName[T] = util.NotGiven[T =:= ""]
  given RCtx(using
      ctName: CTName,
      owner: ResourceOwner = NoResourceOwner
  )(using NonEmptyName[ctName.Out]): RCtx = new RCtx:
    val id = ctName.value
    val owner = owner
