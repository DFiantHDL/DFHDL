//object Test {
//  import scala.annotation.implicitNotFound
//  @implicitNotFound("Bad type here")
//  trait Foo[T <: AnyKind] { type Out ; def id(t: Out): Out = t }
//
//  object Foo {
//    implicit def foo0XInt[T <: Int with Singleton] = new Foo[T] { type Out = T }
//    implicit def foo0XString[T <: String with Singleton] = new Foo[T] { type Out = T }
//    implicit def foo1[T[_]] = new Foo[T] { type Out = T[Any] }
////    implicit def fooFoo[T <: AnyKind, F <: Foo[T]](implicit f : Foo[T]) = new Foo[F] { type Out = F }
////    implicit def foo2[T[_, _]] = new Foo[T] { type Out = T[Any, Any] }
//  }
//
//  def foo[T <: AnyKind](implicit f: Foo[T]): f.type = f
//
//
//  foo[Foo]
//  foo[List].id(List[Any](1, 2, 3))
////  foo[Map].id(Map[Any, Any](1 -> "toto", 2 -> "tata", 3 -> "tutu"))
//}
//import Chisel._
//
//class HelloModule extends Module {
//  val io = new Bundle {}
//  printf("Hello World!\n")
//}
//
//class HelloModuleTests(c: HelloModule) extends Tester(c) {
//  step(1)
//}
//
//object hello {
//  def main(args: Array[String]): Unit = {
//    chiselMainTest(Array[String]("--backend", "c", "--compile", "--test", "--genHarness"),
//      () => Module(new HelloModule())){c => new HelloModuleTests(c)}
//  }
//}

//import scala.language.experimental.macros
//
//trait GenericTrait {
//
//}
//
//trait SpecificTraitA extends GenericTrait {
//  def aaa() = {}
//
//}
//trait SpecificTraitB extends GenericTrait {
//
//}
//
//abstract class AbstractOne {
//}
//
//abstract class AbstractTwo {
//}
//
//
//def doIt: Unit = {
//  val universe: scala.reflect.runtime.universe.type = scala.reflect.runtime.universe
//  import universe._
//  import scala.reflect.runtime.currentMirror
//  import scala.tools.reflect.ToolBox
//  val toolbox = currentMirror.mkToolBox()
//
//  val weakT = weakTypeOf[SpecificTraitA]
//  val weakAbstractOne = weakTypeOf[AbstractOne]
//  val weakAbstractTwo = weakTypeOf[AbstractTwo]
//  val genTree = q"""
//class GenericClass extends $weakT { //T may be limited to GenericTrait's inheriters
//  class GenericSubClassOne extends $weakAbstractOne with $weakT {
//    //...
//  }
//  class GenericSubClassTwo extends $weakAbstractTwo with $weakT {
//    //...
//  }
//}
//    new GenericClass""".asInstanceOf[SpecificTraitA]
//
//  val compiledCode = toolbox.compile(genTree)
//  val result = compiledCode()
//}
//doIt
//
//class SpecificClassA extends GenericClass[SpecificTraitA] {
//
//}
//class SpecificClassB extends GenericClass[SpecificTraitB] {
//
//}
