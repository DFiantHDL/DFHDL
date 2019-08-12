trait Foo {
  trait __Dev {
    lazy val ctx : Int = 0
    final val result = ctx - 1
  }
  lazy val __dev : __Dev = new __Dev {}
//  println(ctx)
}

abstract class Bar(implicit ctx0 : Int) extends Foo {
  trait __DevBar extends __Dev {
    override lazy val ctx = ctx0
  }
  override lazy val __dev : __Dev = new __DevBar {}
}

trait Baz extends Bar

implicit val bb = 5

new Baz {
  println(__dev.result)
}

