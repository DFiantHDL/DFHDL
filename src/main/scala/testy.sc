import DFiant._

trait Oron extends DFDesign {
  val out = DFBool()
  val s1 = DFBool()
  val s2 = DFBool()
  val A = DFBool()
  val B = DFBool()
  val C = DFBool()
  val D = DFBool()

  val o = DFBool().matchdf((s1, s2).bits)
    .casedf(b"00") {A}
    .casedf(b"01") {B}
    .casedf(b"10") {C}
    .casedf(b"11") {D}

  object Color extends Enum.Auto {
    val Red, Blue, Green = Entry
  }

  val color = DFEnum(Color)
  color := Color.Blue

  ifdf (color == Color.Red) {}

  val a = DFBits(8)
  val b = DFBits(8)

  val i = 0
  val mi = DFBits(4).selectdf(a(i))(b, b0s)
}

trait Mul32 extends DFDesign {
  final val a     = DFBits(32) <> IN
  final val b     = DFBits(32) <> IN
  final val tp    = DFBits(32) <> OUT
  final val prod  = DFBits(32) <> OUT
  tp := b0s
  for (i <- 0 until 32) {
    val m = DFBits(32).selectdf(a(i))(b, b0s)
    val sum = (m.uint + tp.uint).wc
    prod(i) := sum(0)
    tp := sum.bits.msbits(32)
  }
}