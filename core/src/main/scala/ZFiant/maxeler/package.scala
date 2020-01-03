package ZFiant

package object maxeler {
  implicit class MaxelerExtras(design : DFDesign) {
    def maxJNode(streamIOs : DFAny.PortOf[_ <: DFAny.Type]*)(scalarIOs : DFAny.PortOf[_ <: DFAny.Type]*) : MaxJNode = {
      MaxJNode(
        design,
        streamIOs.collect{case p : DFAny.Port.In[_,_] => p}.toList,
        streamIOs.collect{case p : DFAny.Port.Out[_,_] => p}.toList,
        scalarIOs.collect{case p : DFAny.Port.In[_,_] => p}.toList,
        scalarIOs.collect{case p : DFAny.Port.Out[_,_] => p}.toList
      )
    }
  }

}
