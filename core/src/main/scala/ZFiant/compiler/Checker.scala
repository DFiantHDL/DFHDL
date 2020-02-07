package ZFiant
package compiler

final class Checker[C](c : C)(implicit comp : Compilable[C]) {
  private val designDB = comp(c)
  import designDB.getset
  def connectionCheck : DFDesign.DB = {
    //      designDB.members.collect {
    //
    //      }
    ???
  }
}
