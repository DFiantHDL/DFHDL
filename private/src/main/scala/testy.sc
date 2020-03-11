val s : Set[Int] = Set(1)

val x : Int = s match {
  case o => o
  case _ => 0
}