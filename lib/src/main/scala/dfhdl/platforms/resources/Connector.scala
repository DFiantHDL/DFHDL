package dfhdl.platforms.resources
import Resource.CanConnect
import dfhdl.DFC

class Connector[T <: Connector.Type, F <: Connector.Form] private (`type`: T, form: F)
    extends ResourceGroup:
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
  sealed abstract class Type(val pinCount: Int)
  abstract class Companion(pinCount: Int):
    object This extends Type(pinCount)
    def Male()(using DFC): Male = Connector(This, Connector.Form.Male)
    type Male = Connector[This.type, Connector.Form.Male.type]
    def Female()(using DFC): Female = Connector(This, Connector.Form.Female)
    type Female = Connector[This.type, Connector.Form.Female.type]

  class Pin private () extends IO
  object Pin:
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
  ]: CanConnect[CM, CF] =
    (male, female) =>
      male.connectFrom(female)
      female.connectFrom(male)
  given femaleMale[
      T <: Type,
      CM <: Connector[T, Form.Male.type],
      CF <: Connector[T, Form.Female.type]
  ]: CanConnect[CF, CM] =
    (female, male) =>
      female.connectFrom(male)
      male.connectFrom(female)
end Connector
