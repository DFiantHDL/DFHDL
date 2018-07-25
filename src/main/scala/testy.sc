val s : List[(Int, Int)] = List()
val s1 = s :+ Tuple2(1, 2)
val s2 = s1 :+ Tuple2(1,2)

s2.distinct