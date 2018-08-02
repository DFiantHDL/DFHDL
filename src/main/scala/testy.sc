val s1 = "oron.port.is.the.best"
val s2 = "oron.port.is.the.best.one"
def relativeName(callFullName : String, refFullName : String) : String = {
  val c = callFullName.split('.')
  val r = refFullName.split('.')
  if (r.length < c.length)
    refFullName
  else {
    val same = c.zip(r).filter(e => e._1 != e._2).isEmpty
    if (same) r.takeRight(r.length-c.length).mkString(".") else ""
  }
}
relativeName(s2,s1)


