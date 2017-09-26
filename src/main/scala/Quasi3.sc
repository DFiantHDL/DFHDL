val universe: scala.reflect.runtime.universe.type = scala.reflect.runtime.universe
import universe._

def foo[T](implicit ev : WeakTypeTag[T]) : Type = weakTypeOf[T]
trait Foo {type Out}
trait Foo2 {type Out}

val a = new Foo {type Out = Int}
val a2 = new Foo {type Out = a.Out}
val aType = foo[a.type]

val bType = aType match {
  case SingleType(pre, sym) => sym.info asSeenFrom (pre, sym.owner)
}

val cType = bType match {
  case NullaryMethodType(tpe) => tpe
}

val dType = cType.member(TypeName("Out")).info

//import DFiant.core._
//import RingExample._
//
/////////////////////////////////////////////////////////////////////////////////////////
////trait DFFlitCtrl extends DFStruct[DFFlitCtrl] {
////  val a = insert(DFBits(8))
////  val b = insert(DFBits(8))
//////  val nodesNum : Int
////}
//
//object Builder {
//
//  def helper[Val <: DFAny : c.WeakTypeTag]
//  (c : Context) : c.Expr[Val] = {
//    import c.universe._
//    val weakVal = c.weakTypeOf[Val]
//  //  reify{showRaw(weakVal)}
////    val weakR = c.weakTypeOf[STR]
////    val parents = List(q"$weakR", q"$weakVal")
//    //val genTree = q"new ..$parents {}"
////    //showRaw(parents)
//    val genTree =
//    q"""
//        new DFStruct.Var[$weakVal] with $weakVal {}
//      """
//    c.Expr(genTree)
////    import _root_.DFiant.core._
//    //        import _root_.RingExample._
//    // { ..$body }
//    //showRaw(genTree)
////    val compiledCode = toolbox.eval(genTree)
////    compiledCode.asInstanceOf[STR with Val]
////    genTree.asInstanceOf[c.Expr[STR with Val]]
//  }
//  def apply[Val <: DFAny] : DFStruct.Var[Val] with Val = macro helper[Val]
//}
//
//val b = Builder[DFFlitCtrl]
//reify()
////val weakVal = weakTypeOf[DFFlitCtrl]
////val weakR = weakTypeOf[DFStruct.Var[DFFlitCtrl]]
////val parents = List(q"$weakR", q"$weakVal")
////val body2 = {val nodesNum : Int = 0}
////val body = reify({val nodesNum : Int = 0})
////val genTree = q"new ..$parents { ..$body }"
////
