package ZFiant

package object maxeler {
  implicit class MaxelerExtras(design : DFDesign) {
    def maxJNode : MaxJNode = MaxJNode(design)
  }

  implicit class PortsTagging[P <: DFAny.PortOf[_ <: DFAny.Type]](port : P) {
    def setMaxelerStreamIOPush(implicit getSet: MemberGetSet) : Unit = port.addCustomTag(MaxelerStreamIOPush)
    def setMaxelerStreamIOPull(implicit getSet: MemberGetSet) : Unit = port.addCustomTag(MaxelerStreamIOPull)
    def setMaxelerScalarIO(implicit getSet: MemberGetSet) : Unit = port.addCustomTag(MaxelerScalarIO)
  }

}
