trait JustATrait {
  type T <: JustATrait
  def createAnother : JustAClass[T]
}

trait JustATraitExt[J <: JustATrait] extends JustATrait {
//  this : J =>
  type T = J
}


import scala.reflect.ClassTag
import scala.reflect.runtime.universe._
class JustAClass[J <: JustATrait : ClassTag] extends JustATraitExt[J] {
////  this : J =>
//  val universe: scala.reflect.runtime.universe.type = scala.reflect.runtime.universe
  import scala.reflect._
//  import universe._
//  import scala.reflect.runtime.{currentMirror => m}
//  import scala.tools.reflect.ToolBox
//  val toolbox = m.mkToolBox()
//
//  def newInstance[T : TypeTag] = {
//    val clazz = typeTag[T].mirror.runtimeClass(typeOf[T])
//    clazz.newInstance.asInstanceOf[T]
//  }
  def createAnother : JustAClass[T] = {
    this.getClass().newInstance().asInstanceOf[JustAClass[T]]
    //new JustAClass[T]
  }

}

trait Me extends JustATraitExt[Me]

val j = new JustAClass[Me]
val jj = j.createAnother