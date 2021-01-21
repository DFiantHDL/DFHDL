package DFiant.internals

import singleton.ops._

class ExactTypeSpec extends munit.FunSuite {
  case class Positive[P <: Int](value: P)
  trait ID
  object Positive extends ExactType[ID]
  def foo(
      positive: Positive.Conv[None.type, Positive[_ <: Int]]
  ): positive.Out = positive(None)

  implicit def positiveSummon[T <: XInt](implicit
      req: RequireMsg[T > 0, "Must be positive. Got " + ToString[
        T
      ] + " instead"]
  ): Positive.Summon.SAM[None.type, T, Positive[T]] = (_, t) => Positive(t)

  val one: Positive[1] = foo(1)
  test("Value should be available") {
    assert(compileErrors("""val one : Positive[1] = foo(1)""").isEmpty)
  }
  test("Value should return proper error") {
    assertNoDiff(
      compileErrors("""foo(-1)"""),
      """|error: Must be positive. Got -1 instead
                                                  |foo(-1)
                                                  |    ^
                                                  |""".stripMargin
    )
  }


  case class PositiveTuple[P1 <: Int, P2 <: Int](value : Tuple2[P1, P2])
  trait IDTuple
  object PositiveTuple extends ExactType[IDTuple]
  def fooTuple(positive : PositiveTuple.Conv[None.type, PositiveTuple[_ <: Int, _ <: Int]]) : positive.Out = positive(None)

  implicit def positiveTupleSummon[T1 <: XInt, T2 <: XInt](
    implicit
    req1 : RequireMsg[T1 > 0, "Must be positive. Got " + ToString[T1] + " instead"],
    req2 : RequireMsg[T2 > 0, "Must be positive. Got " + ToString[T2] + " instead"]
  ) : PositiveTuple.Summon.SAM[None.type, Tuple2[T1, T2], PositiveTuple[T1, T2]] = (_, t) => PositiveTuple(t)

  val oneTuple : PositiveTuple[1, 2] = fooTuple(Tuple2(1, 2))
  test("Tuple value should be available") {
    assert(compileErrors("""val one : PositiveTuple[1, 2] = fooTuple((1, 2))""").isEmpty)
  }
  test("Tuple value should return proper error") {
    assertNoDiff(
      compileErrors("""fooTuple((-1, 2))"""),
      """|error: Must be positive. Got -1 instead
         |fooTuple((-1, 2))
         |         ^
         |""".stripMargin
    )
    assertNoDiff(
      compileErrors("""fooTuple((1, -2))"""),
      """|error: Must be positive. Got -2 instead
         |fooTuple((1, -2))
         |         ^
         |""".stripMargin
    )
  }

  implicit def evenSummon[T <: XInt](
    implicit req : RequireMsg[(T % 2) == 0, "Must be even. Got " + ToString[T] + " instead"]
  ) : Positive.Summon.SAM[None.type, T, Int] = (_, t) => t

  def fooVarArgs(even: Positive.Conv[None.type, Int]*): Seq[Int] =
    even.map(_.apply(None))

  test("VarArgs Value should be available") {
    val seq = fooVarArgs(2, 4, 6, 8)
    assert(seq equals Seq(2, 4, 6, 8))
  }
  test("VarArgs Value should return proper error") {
    assertNoDiff(
      compileErrors("""fooVarArgs(2,4,5,8)"""),
      """|error: Must be even. Got 5 instead
                                                              |fooVarArgs(2,4,5,8)
                                                              |               ^
                                                              |""".stripMargin
    )
  }
}
