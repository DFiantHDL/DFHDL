import oscar.cp._

object BasicOptimizationModel extends CPModel with App {



  def go = {
    val x1 = CPIntVar(Set(1,2,3,9))
    val x2 = CPIntVar(1 to 5)
    val x3 = CPIntVar(1 until 5)


    add(x1 !== x2)
    add(x1 + x2 === x3)

    // define an objective function
    maximize(x1+x3)

    search {
      binaryFirstFail(Seq(x1,x2))
    } onSolution {
      println(s"Solution found $x1 $x2 $x3")
    }

    start()
  }
}

BasicOptimizationModel.go
//MyModel.go