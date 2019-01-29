case class Nummy[N <: Int : Singleton]()

val a = Nummy[1]()