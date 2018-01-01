val l1 = Seq(1, 2)
val l2 = Seq(11, 12, Seq(1, 2))

def flatten(l: Seq[Any]): Seq[Any] = l flatMap {
  case ls: Seq[_] => flatten(ls)
  case h => List(h)
}

flatten(l2)