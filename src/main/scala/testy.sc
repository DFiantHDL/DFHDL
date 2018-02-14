import DFiant._
import GlobalDesign._

def foo() : Unit = {println("shit")}

def fooUser(in : => Unit): Unit ={
  val a = in
  val b = in
}

fooUser(foo)