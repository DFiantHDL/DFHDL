package ZFiant

import compiler.Compilable

package object maxeler {
  implicit def evMaxJNodeOps[D <: DFDesign, S <: shapeless.HList, C](c : C)(implicit conv : C => Compilable[D, S])
  : MaxJNodeOps[D, S] = new MaxJNodeOps[D, S](c)

  implicit class PortsTagging[P <: DFAny.PortOf[_ <: DFAny.Type]](port : P) {
    def setMaxelerStreamIOPush(implicit getSet: MemberGetSet) : Unit = port.addCustomTag(MaxelerStreamIOPush)
    def setMaxelerStreamIOPull(implicit getSet: MemberGetSet) : Unit = port.addCustomTag(MaxelerStreamIOPull)
    def setMaxelerScalarIO(implicit getSet: MemberGetSet) : Unit = port.addCustomTag(MaxelerScalarIO)
  }

}
