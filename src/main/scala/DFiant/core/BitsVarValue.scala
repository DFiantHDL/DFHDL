package DFiant.core

protected[DFiant] class BitsVarValue(_width : Int) {
  val width = _width
  def copy(bitHigh : Int = _width-1, bitLow : Int = 0) : BitsVarValue = {this}
}

protected[DFiant] class BitsVarValues(_width : Int) {
  val width = _width
  def getBitsVar(bitHigh : Int = _width-1, bitLow : Int = 0) : BitsVarValue = new BitsVarValue(width)
  def setBitsVar(bitsVarValue : BitsVarValue, bitHigh : Int = _width-1, bitLow : Int = 0) : Unit = {}
}

