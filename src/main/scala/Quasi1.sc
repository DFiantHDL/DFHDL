val universe: scala.reflect.runtime.universe.type = scala.reflect.runtime.universe
import universe._
import scala.reflect.runtime.{currentMirror => m}
import scala.tools.reflect.ToolBox
val toolbox = m.mkToolBox()

trait Father
class A extends Father {
  println("I'm A")
  val a = 0
}
class B extends Father {
  println("I'm B")
  val b = 0
}

object Produce {
  def A(): A = {
    val weakT = weakTypeOf[A]
    val genTree = q"""
      case class Son() extends $weakT {
        println("I'm alive")
      }
      Son()
      """
    val compiledCode = toolbox.eval(genTree)
    compiledCode.asInstanceOf[A]
  }
  def apply[T <: Father : TypeTag](): T = {
    val weakT = weakTypeOf[T]
    val genTree = q"""
      case class Son() extends $weakT {
        println("I'm alive")
      }
      Son()
      """
    val compiledCode = toolbox.eval(genTree)
    compiledCode.asInstanceOf[T]
  }
}

Produce.A()

Produce[A]()

