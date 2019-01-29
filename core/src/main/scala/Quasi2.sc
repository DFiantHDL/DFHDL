val universe: scala.reflect.runtime.universe.type = scala.reflect.runtime.universe
import scala.reflect._
import universe._
import scala.reflect.runtime.{currentMirror => m}
import scala.tools.reflect.ToolBox
val toolbox = m.mkToolBox()

trait Father {
  type F = Father
  def ext[T <: Father : TypeTag]: T = {
    val weakT = typeOf[T]
    val genTree = q"""
      case class Son() extends $weakT {
        println("I'm alive")
      }
      Son()
      """
    println(genTree)
    val compiledCode = toolbox.eval(genTree)
    compiledCode.asInstanceOf[T]
  }
  def extMe() = ext[F]
}

val f = new Father {}
f.extMe()

//class Father {
//  val fatherName = "Father"
//  println("I'm a " + fatherName)
//
//
//}
//
//class A extends Father {
//  println("I'm A")
//  val a = 0
//}
//class B extends Father {
//  println("I'm B")
//  val b = 0
//}
//
//object Produce {
//  def A(): A = {
//    val weakT = weakTypeOf[A]
//    val genTree = q"""
//      case class Son() extends $weakT {
//        println("I'm alive")
//      }
//      Son()
//      """
//    val compiledCode = toolbox.eval(genTree)
//    compiledCode.asInstanceOf[A]
//  }
//}
//
//Produce.A()
//
//Produce[A]()
//
