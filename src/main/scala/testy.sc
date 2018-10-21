import DFiant._

//def fa(a, b, c_in) = {
//  s = (a ^ b)^ c_in
//  c_out = (a & b) | (c_in & (a ^ b))
//  return (c_out, s)
//}

def fa(a : DFBool, b : DFBool, c_in : DFBool)(implicit ctx : DFDesign.ContextOf[_]) : (DFBool, DFBool) = {
  val s = (a ^ b) ^ c_in
  val c_out = (a && b) || (c_in && (a ^ b))
  return (c_out, s)
}
