val s1 = "oron.port.is.the.best"
val s2 = "oron.port.is.te"
def comp(callFullName : String, refFullName : String) : String = {
  val c = callFullName.split('.')
  val r = refFullName.split('.')
  if (r.length < c.length)
    return refFullName

  val res = for {i <- (math.max(c.length, r.length)-1) to 0 by -1; if c.length <= i || (c(i) != r(i))} yield r(i)

  res.mkString(".")


}
comp(s2,s1)