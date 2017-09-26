
//case class Foo[N <: Int with Singleton]()
//@unroll for(i <- 0 to 5)
//  Foo[i]()
//
//inline def inFoo(num : Int) : Int = ???
//@unroll for(i <- 0 to 5)
//  Foo[inFoo(i)]()
//
//@unroll for(i <- 0 to 5)
//  @unroll for (j <- 0 to inFoo(i))
//    Foo[j]()

case class Width(width : Int)

sealed trait BitsRange {
  def width(implicit w : Width) : Int
}

object AllBits extends BitsRange {
  def width(implicit w : Width) : Int = w.width
}
implicit val w = Width(5)
val a = AllBits.width
