final class DependencyBox[T](value : => T) {
  private var visited : Boolean = false
  lazy val valueOption : Option[T] = {
    if (visited) None
    else {
      visited = true
      Some(value)
    }
  }
}
object DependencyBox {
  def apply[T](value : => T) : DependencyBox[T] = new DependencyBox[T](value)
}



object A {
  val db = DependencyBox(5 * B.value)
  lazy val value:Int = db.valueOption.getOrElse(throw new IllegalArgumentException("This is bad"))
}

object B {
  val db = DependencyBox(3 * A.value)
  lazy val value:Int = 1//db.valueOption.getOrElse(throw new IllegalArgumentException("This is bad"))
}

println("A= "+A.value) // 8
println("B = "+B.value) // 3