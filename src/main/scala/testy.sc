def relativePath(refFullPath : String, callFullPath : String) : String = {
  val c = callFullPath.split('.')
  val r = refFullPath.split('.')
  if (r.length < c.length) {
    val idx = r.zip(c).indexWhere(e => e._1 != e._2)
    if (idx == -1) "" else r.takeRight(c.length-idx-1).mkString(".")
  } else {
    val idx = c.zip(r).indexWhere(e => e._1 != e._2)
    if (idx == -1) r.takeRight(r.length-c.length).mkString(".") else r.takeRight(r.length-idx).mkString(".")
  }
}
val ref = "oron.port.it.th.best"
val call = "oron.port.is"
relativePath(ref,call)