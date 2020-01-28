class DFAny
trait Owner
trait Ref[+T <: DFAny] {
  def get : T = Ref.get(this)
}
import collection.mutable
object Ref {
  val db = mutable.Map.empty[Ref[_], DFAny]
  def add[T <: DFAny, R <: Ref[T]](t : T, ref : R) : R = {
    db += (ref -> t)
    ref
  }
  implicit def get[T <: DFAny](ref : Ref[T]) : T = db(ref).asInstanceOf[T]
}

trait OwnedRef[T <: DFAny] extends Ref[T] {
  val owner : Ref[DFAny]
}
object OwnedRef {
  implicit def apply[T <: DFAny](f : T)(implicit owner0 : => DFAny) : OwnedRef[T] = Ref.add(f, new OwnedRef[T] {
    lazy val owner : Ref[DFAny] = Ref.add(owner0, new Ref[DFAny]{})
  })
}
class DFAnyUser(val any : OwnedRef[DFAny]) extends DFAny

val f1 = new DFAny
implicit lazy val f1user : DFAnyUser = new DFAnyUser(OwnedRef(f1))
f1user.any.owner.get
