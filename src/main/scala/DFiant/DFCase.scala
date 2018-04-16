package DFiant

trait DFCase[A] {
  def casedf(a : A*)(block : => Unit) : DFCase[A] = ???
  def case_(block : => Unit) : Unit = {}

}
