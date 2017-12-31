val l1 = Seq(1, 2, 3)
val l2 = Seq(11, 12)

val filly = Seq.fill(1)(l2.head)
val L1 = l1
val L2 = filly ++ l2

L1.zip(L2).map(t => t._1 + t._2)


