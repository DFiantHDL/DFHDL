package ZFiant

package object maxeler {
  implicit class MaxelerExtras(design : DFDesign) {
    def maxJNode(pullIOs : DFAny.PortOf[_ <: DFAny.Type]*)(pushIOs : DFAny.PortOf[_ <: DFAny.Type]*)(scalarIOs : DFAny.PortOf[_ <: DFAny.Type]*) : MaxJNode = {
      MaxJNode(
        design,
        pullIOs.collect{case p : DFAny.Port.In[_,_] => p}.toList,
        pullIOs.collect{case p : DFAny.Port.Out[_,_] => p}.toList,
        pushIOs.collect{case p : DFAny.Port.In[_,_] => p}.toList,
        pushIOs.collect{case p : DFAny.Port.Out[_,_] => p}.toList,
        scalarIOs.collect{case p : DFAny.Port.In[_,_] => p}.toList,
        scalarIOs.collect{case p : DFAny.Port.Out[_,_] => p}.toList
      )
    }
  }

}
