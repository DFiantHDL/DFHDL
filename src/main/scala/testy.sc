import DFiant._
import singleton.ops._
import scodec.bits._
import DFiant.internals._
import shapeless._

trait A
object A {
  implicit def conv(a : A) : B with C = new B with C {}
  implicit def convfake(a : A) : B = ???
}

trait B
trait C


val a = new A{}
val bc : B with C = a
val b : B = a