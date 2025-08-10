package dfhdl.platforms.resources
import Resource.CanConnect
import dfhdl.DFC

class Connector[T <: Connector.Type, F <: Connector.Form](`type`: T, form: F) extends ResourceGroup:
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
  abstract class Type(val pinCount: Int):
    type This <: Type
    def Male()(using DFC): Male = Connector(this.asInstanceOf[This], Connector.Form.Male)
    type Male = Connector[This, Connector.Form.Male.type]
    def Female()(using DFC): Female = Connector(this.asInstanceOf[This], Connector.Form.Female)
    type Female = Connector[This, Connector.Form.Female.type]

  class Pin private () extends IO
  object Pin:
    def apply()(using DFC): Pin = new Pin

  given [
      T <: Type,
      CM <: Connector[T, Form.Male.type],
      CF <: Connector[T, Form.Female.type]
  ]: CanConnect[CM, CF] =
    (male, female) =>
      male.connectFrom(female)
      female.connectFrom(male)
end Connector
