val l1 = Seq(1, 2)
val l2 = Seq(11, 12, 13)

val filly = Seq.fill(1)(l2.head)
val L1 = l1
val L2 = filly ++ l2

l1.zipAll(l2, l1.last, l2.last).map(t => t._1 + t._2)


