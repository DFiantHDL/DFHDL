trait Person {
  type P <: Person
  def playWith(person : P) = {}
}

trait DuplicatingPerson extends Person {
  type DP <: Person
  type DPP <: DP#P
  def duplicate(personToDuplicate : DP) : DPP = personToDuplicate.asInstanceOf[DPP]
}

trait Scientist extends DuplicatingPerson {
  type P = Scientist
}

trait MadScientist extends DuplicatingPerson {
  type P = MadScientist
  type DP = Scientist
  val scientist = duplicate(new Scientist {})
}

val scientist = new Scientist {}
val madScientist = new MadScientist {}
madScientist.scientist.playWith(scientist)


case class A() {
  this.
}
