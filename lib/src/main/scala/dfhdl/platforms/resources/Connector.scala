package dfhdl.platforms.resources
import Resource.CanConnect
import dfhdl.DFC
import dfhdl.internals.metaContextIgnore

class Connector[T <: Connector.Type, F <: Connector.Form] private (
    protected[Connector] val `type`: T,
    form: F
) extends ResourceGroup:
  protected val pins: List[Connector.Pin] =
    List.tabulate(`type`.pinCount)(i => Connector.Pin()(using dfc.setName(s"P${i + 1}")))

  /** @param i
    *   pin number, 1-based
    * @return
    *   pin
    */
  def P(i: Int): Connector.Pin = pins(i - 1)

object Connector:
  enum Form derives CanEqual:
    case Male, Female
  sealed abstract class Type(val pinCount: Int, val pinMap: Int => Int)
  abstract class Companion(pinCount: Int):
    class This(pinMap: Int => Int) extends Type(pinCount, pinMap)
    protected def Male(pinMap: Int => Int)(using DFC): Male =
      Connector(This(pinMap), Connector.Form.Male)
    def Male()(using DFC): Male = Male(identity)
    type Male = Connector[This, Connector.Form.Male.type]
    protected def Female(pinMap: Int => Int)(using DFC): Female =
      Connector(This(pinMap), Connector.Form.Female)
    def Female()(using DFC): Female = Female(identity)
    type Female = Connector[This, Connector.Form.Female.type]

  class Pin private () extends IO
  object Pin:
    @metaContextIgnore
    def apply()(using DFC): Pin = new Pin

  inline given [
      T <: Type,
      F <: Form,
      C1 <: Connector[T, F],
      C2 <: Connector[T, F]
  ]: CanConnect[C1, C2] =
    compiletime.error("Cannot connect connectors of the same form (must be male and female)")

  given maleFemale[
      T <: Type,
      CM <: Connector[T, Form.Male.type],
      CF <: Connector[T, Form.Female.type]
  ]: CanConnect[CM, CF] with
    def connect(male: CM, female: CF)(using DFC): Unit =
      for (i <- 1 to male.`type`.pinCount)
        male.P(male.`type`.pinMap(i)) <> female.P(female.`type`.pinMap(i))
  given femaleMale[
      T <: Type,
      CM <: Connector[T, Form.Male.type],
      CF <: Connector[T, Form.Female.type]
  ]: CanConnect[CF, CM] with
    def connect(female: CF, male: CM)(using DFC): Unit =
      for (i <- 1 to male.`type`.pinCount)
        male.P(male.`type`.pinMap(i)) <> female.P(female.`type`.pinMap(i))
end Connector
