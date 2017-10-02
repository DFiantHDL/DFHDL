package DFiant.core

import scala.collection.immutable._

import singleton.twoface._
sealed trait DFArray[E <: DFAny] extends DFAny.Val[WUnsafe, DFArray[E], DFArray.Var[E]] {
  type TElem = E
  type TAliasElem <: TElem#TVal

  val length : Int
  protected val dfVar : TElem#TVar

  val width = length * dfVar.width.getValue
  private val arr = Vector.tabulate[TAliasElem](length)(_ => dfVar.newEmptyDFVar.asInstanceOf[TAliasElem])
  def apply(i : Int) : TAliasElem = arr(i)
  def dfTypeName : String = "DFArray"
}


object DFArray {
  case class Var[E <: DFAny](length : Int, protected val dfVar : E#TVar) extends DFAny.Var[WUnsafe, DFArray[E], DFArray.Var[E]] with DFArray[E] {
    type TAliasElem = TElem#TVar
    def newEmptyDFVar = copy()
  }

  def apply(length : Int, dfVar : DFAny) = Var[dfVar.TVal](length, dfVar.asInstanceOf[dfVar.TVal#TVar])
}



object BB {
  val a = DFArray(2,DFArray(5,DFBool()))
//  a(0)(0) := false
}