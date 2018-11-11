import DFiant._

val a = Seq(1,2,3)
val b = Seq(5, 6, 7)

val lists = Seq(a, b)

lists.flatMap(_.zipWithIndex).sortBy(_._2).map(_._1)