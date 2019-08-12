trait DSLMemberConstruct
trait DSLOwnerConstruct extends DSLMemberConstruct


trait Foo {
  trait __Dev {
    lazy val ownerOption : Option[Int] = ???
    final val result = ownerOption.get - 1
  }
  lazy val __dev : __Dev = new __Dev {}
//  println(ctx)
}

abstract class Bar(implicit ownerOption0 : Option[Int]) extends Foo {
  trait __DevBar extends __Dev {
    override lazy val ownerOption = ownerOption0
  }
  override lazy val __dev : __Dev = new __DevBar {}
}

trait Baz extends Bar

implicit val bb = Some(5)

new Baz {
  println(__dev.result)
}

