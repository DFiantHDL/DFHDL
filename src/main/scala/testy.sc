
import shapeless._
import scala.languageFeature.implicitConversions

object Bla {

  object Values {
    implicit def conv[T](self: this.type)(implicit v: MkValues[T]): Set[T] = Values[T]

    def apply[T](implicit v: MkValues[T]): Set[T] = v.values.toSet

    trait MkValues[T] {
      def values: List[T]
    }

    object MkValues {
      implicit def values[T, Repr <: Coproduct]
      (implicit gen: Generic.Aux[T, Repr], v: Aux[T, Repr]): MkValues[T] =
        new MkValues[T] {
          def values = v.values
        }

      trait Aux[T, Repr] {
        def values: List[T]
      }

      object Aux {
        implicit def cnilAux[A]: Aux[A, CNil] =
          new Aux[A, CNil] {
            def values = Nil
          }

        implicit def cconsAux[T, L <: T, R <: Coproduct]
        (implicit l: Witness.Aux[L], r: Aux[T, R]): Aux[T, L :+: R] =
          new Aux[T, L :+: R] {
            def values = l.value :: r.values
          }
      }

    }

  }


  trait Counter {
    var value : Int = 0
    def inc : Unit = value = value + 1
  }
  implicit val cnt = new Counter {}

  abstract class Entry(implicit cnt : Counter) {
    val value = cnt.value
    cnt.inc
    override def toString: String = value.toString
  }

  sealed trait WeekDay extends Entry {
  }

  object WeekDay {
    val Mon, Tue, Wed, Thu, Fri, Sat, Sun = new WeekDay {}
    val values: Set[WeekDay] = Values
  }

}


Bla.WeekDay.Mon.value
Bla.WeekDay.Tue.value