package CoreSpec
import dfhdl.*
import munit.*

class DFIfSpec extends DFSpec:
  val i = Boolean <> IN
  val x = UInt(8) <> VAR

  test("No ret val") {
    assertCodeString(
      """|if (i) x := d"8'1"
         |else if (!i) x := d"8'2"
         |""".stripMargin
    ) {
      if (i) x := 1
      else if (!i) x := 2
    }
    assertCodeString(
      """|if (i) x := d"8'1"
         |""".stripMargin
    ) {
      if (i) x := 1
    }
    assertCodeString(
      """|if (i) x := d"8'1"
         |else if (!i)
         |  x := d"8'2"
         |  x := d"8'22"
         |else if (i || i) x := d"8'3"
         |""".stripMargin
    ) {
      if (i) x := 1
      else if (!i)
        x := 2
        x := 22
      else if (i || i)
        x := 3
    }
    assertCodeString(
      """|if (i) x := d"8'1"
         |else if (!i)
         |  x := d"8'2"
         |  x := d"8'22"
         |else if (i || i) x := d"8'3"
         |else x := d"8'7"
         |""".stripMargin
    ) {
      if (i) x := 1
      else if (!i)
        x := 2
        x := 22
      else if (i || i)
        x := 3
      else x := 7
    }
    assertCodeString(
      """|if (i) x := d"8'1"
         |else if (!i)
         |  if (i) x := d"8'1"
         |  else x := d"8'7"
         |else if (i || i) x := d"8'3"
         |else x := d"8'7"
         |""".stripMargin
    ) {
      if (i) x := 1
      else if (!i)
        if (i) x := 1
        else x := 7
      else if (i || i)
        x := 3
      else x := 7
    }
  }

  test("With ret val") {
    assertCodeString(
      """|val res: UInt[8] <> VAL =
         |  if (i) d"8'1"
         |  else if (!i) x.bits.uint
         |  else d"8'2"
         |""".stripMargin
    ) {
      val res: UInt[8] <> VAL =
        if (i) 1
        else if (!i) x.bits.uint
        else 2
    }
    assertCodeString(
      """|val res: UInt[8] <> VAL =
         |  if (i)
         |    if (i) d"8'1"
         |    else d"8'2"
         |  else if (!i) x.bits.uint
         |  else d"8'3"
         |""".stripMargin
    ) {
      val res: UInt[8] <> VAL =
        if (i)
          if (i) 1
          else 2
        else if (!i) x.bits.uint
        else 3
    }
    assertCodeString(
      """|val res: UInt[8] <> VAL =
         |  if (i)
         |    val internal: UInt[8] <> VAL =
         |      if (i) d"8'1"
         |      else d"8'2"
         |    internal
         |  else if (!i) x.bits.uint
         |  else d"8'3"
         |""".stripMargin
    ) {
      val res: UInt[8] <> VAL =
        if (i)
          val internal: UInt[8] <> VAL =
            if (i) 1
            else 2
          internal
        else if (!i) x.bits.uint
        else 3
    }
    assertCodeString(
      """|val res =
         |  ((
         |    if (i) i
         |    else i
         |  ): Boolean <> VAL) || ((
         |    if (!i) !i
         |    else !i
         |  ): Boolean <> VAL)
         |""".stripMargin
    ) {
      val res: Boolean <> VAL =
        ((if (i) i else i): Boolean <> VAL) ||
          ((if (!i) !i else !i): Boolean <> VAL)
    }
    assertCodeString(
      """|val rs0: Boolean <> VAL =
         |  if (!i) !i
         |  else !i
         |val res =
         |  ((
         |    if (i) i
         |    else i
         |  ): Boolean <> VAL) || rs0
         |""".stripMargin
    ) {
      val rs0 = ((if (!i) !i else !i): Boolean <> VAL)
      val res: Boolean <> VAL =
        ((if (i) i else i): Boolean <> VAL) ||
          rs0
    }
  }
end DFIfSpec
