package app
import dfhdl.*
import dfhdl.compiler.ir
import dfhdl.core.{DFC, DFVal, DFBits, DFUInt, DFSInt}
import dfhdl.core.r__For_Plugin.defaults
import dfhdl.app.DesignArg
import munit.*

class DesignArgSpec extends FunSuite:
  given dfc: DFC = DFC.emptyNoEO

  private def arg(name: String, value: Any): DesignArg = DesignArg(name, value, "")

  // ----------------------------------------------------------------
  // typeName routing
  // ----------------------------------------------------------------
  test("typeName routes each supported type correctly"):
    assertEquals(arg("a", defaults.int32).typeName, "Int")
    assertEquals(arg("b", defaults.bool).typeName, "Boolean")
    assertEquals(arg("c", defaults.bit).typeName, "Bit")
    assertEquals(arg("d", defaults.string).typeName, "String")
    assertEquals(arg("e", defaults.double).typeName, "Double")
    assertEquals(arg("f", defaults.bits[8](8)).typeName, "Bits")
    assertEquals(arg("g", defaults.uint[16](16)).typeName, "UInt")
    assertEquals(arg("h", defaults.sint[32](32)).typeName, "SInt")
    // Plain Scala values also route correctly (used when a legacy default
    // slips in as a raw Scala value rather than a DFVal).
    assertEquals(arg("i", 1).typeName, "Int")
    assertEquals(arg("j", true).typeName, "Boolean")
    assertEquals(arg("k", "").typeName, "String")
    assertEquals(arg("l", 1.0).typeName, "Double")

  // ----------------------------------------------------------------
  // defaultDisplay — DFHDL-literal display
  // ----------------------------------------------------------------
  test("defaultDisplay renders DFHDL literal form for synthetic defaults"):
    assertEquals(arg("a", defaults.int32).defaultDisplay, "0")
    assertEquals(arg("b", defaults.bool).defaultDisplay, "false")
    assertEquals(arg("c", defaults.bit).defaultDisplay, "0")
    // Guards the empty-String display regression — must show `""`, not an
    // empty placeholder.
    assertEquals(arg("d", defaults.string).defaultDisplay, "\"\"")
    assertEquals(arg("e", defaults.double).defaultDisplay, "0.0")
    assertEquals(arg("f", defaults.bits[8](8)).defaultDisplay, "h\"??\"")
    assertEquals(arg("g", defaults.uint[16](16)).defaultDisplay, "d\"16'0\"")
    assertEquals(arg("h", defaults.sint[32](32)).defaultDisplay, "sd\"32'0\"")

  test("defaultDisplay for user-written constants reflects the stored value"):
    val i42 = DFVal.Const.forced(dfhdl.core.DFInt32, Some(BigInt(42)))
    assertEquals(arg("a", i42).defaultDisplay, "42")
    val s = DFVal.Const.forced(dfhdl.core.DFString, Some("hello"))
    assertEquals(arg("d", s).defaultDisplay, "\"hello\"")
    val d = DFVal.Const.forced(dfhdl.core.DFDouble, Some(2.5))
    assertEquals(arg("e", d).defaultDisplay, "2.5")

  // ----------------------------------------------------------------
  // isSyntheticDefault — distinguishes `defaults.*` from user-written consts
  // ----------------------------------------------------------------
  test("isSyntheticDefault is true for synthetic defaults, false for user consts"):
    assert(arg("a", defaults.int32).isSyntheticDefault)
    assert(arg("f", defaults.bits[8](8)).isSyntheticDefault)
    // User-written const — no tag.
    val userInt = DFVal.Const.forced(dfhdl.core.DFInt32, Some(BigInt(7)))
    assert(!arg("a", userInt).isSyntheticDefault)
    val userStr = DFVal.Const.forced(dfhdl.core.DFString, Some("hi"))
    assert(!arg("d", userStr).isSyntheticDefault)

  // ----------------------------------------------------------------
  // updateScalaValue round-trips for each type
  // ----------------------------------------------------------------
  test("updateScalaValue round-trips Int"):
    val updated = arg("a", defaults.int32).updateScalaValue(5)
    assertEquals(updated.getScalaValue, 5)
    assertEquals(updated.defaultDisplay, "5")

  test("updateScalaValue round-trips Boolean (both from Boolean and String)"):
    assertEquals(arg("b", defaults.bool).updateScalaValue(true).getScalaValue, true)
    assertEquals(arg("b", defaults.bool).updateScalaValue("true").getScalaValue, true)
    assertEquals(arg("b", defaults.bool).updateScalaValue("false").getScalaValue, false)

  test("updateScalaValue round-trips Bit from '0'/'1'"):
    // For Bit the DFHDL literal form is "0"/"1", so `getScalaValue` returns
    // the same representation the user typed (and `defaultDisplay` matches).
    assertEquals(arg("c", defaults.bit).updateScalaValue("1").defaultDisplay, "1")
    assertEquals(arg("c", defaults.bit).updateScalaValue("0").defaultDisplay, "0")

  test("updateScalaValue round-trips String"):
    val updated = arg("d", defaults.string).updateScalaValue("hello")
    assertEquals(updated.getScalaValue, "hello")
    assertEquals(updated.defaultDisplay, "\"hello\"")

  test("updateScalaValue round-trips Double"):
    val updated = arg("e", defaults.double).updateScalaValue(2.5)
    assertEquals(updated.getScalaValue, 2.5)

  test("updateScalaValue round-trips Bits[8] (quoted and bare forms)"):
    val quoted = arg("f", defaults.bits[8](8)).updateScalaValue("h\"5a\"")
    assertEquals(quoted.defaultDisplay, "h\"5a\"")
    val bareHex = arg("f", defaults.bits[8](8)).updateScalaValue("5a")
    assertEquals(bareHex.defaultDisplay, "h\"5a\"")
    val bin = arg("f", defaults.bits[8](8)).updateScalaValue("b\"10101010\"")
    assertEquals(bin.defaultDisplay, "h\"aa\"")

  test("updateScalaValue round-trips UInt[16] (quoted and bare forms)"):
    val quoted = arg("g", defaults.uint[16](16)).updateScalaValue("d\"16'100\"")
    assertEquals(quoted.defaultDisplay, "d\"16'100\"")
    val bare = arg("g", defaults.uint[16](16)).updateScalaValue("100")
    assertEquals(bare.defaultDisplay, "d\"16'100\"")

  test("updateScalaValue round-trips SInt[32] (quoted and bare forms)"):
    val quoted = arg("h", defaults.sint[32](32)).updateScalaValue("sd\"32'-42\"")
    assertEquals(quoted.defaultDisplay, "sd\"32'-42\"")
    val bare = arg("h", defaults.sint[32](32)).updateScalaValue("-42")
    assertEquals(bare.defaultDisplay, "sd\"32'-42\"")

  // ----------------------------------------------------------------
  // Identity shortcut when the CLI value equals the current default
  // ----------------------------------------------------------------
  test("updateScalaValue returns the same DesignArg when the value is unchanged"):
    val original = arg("a", defaults.int32)
    val updated = original.updateScalaValue(0) // default is 0
    assert(updated eq original)
    // And the synthetic tag is therefore preserved.
    assert(updated.isSyntheticDefault)

  // ----------------------------------------------------------------
  // Malformed inputs throw IllegalArgumentException with the arg name
  // ----------------------------------------------------------------
  test("malformed Bit literal throws IllegalArgumentException with arg name"):
    val ex = intercept[IllegalArgumentException]:
      arg("c", defaults.bit).updateScalaValue("2")
    assert(clue(ex.getMessage).contains("c"))

  test("malformed Boolean literal throws IllegalArgumentException with arg name"):
    val ex = intercept[IllegalArgumentException]:
      arg("b", defaults.bool).updateScalaValue("yes")
    assert(clue(ex.getMessage).contains("b"))

  test("malformed Bits literal throws IllegalArgumentException with arg name"):
    val ex = intercept[IllegalArgumentException]:
      arg("f", defaults.bits[8](8)).updateScalaValue("h\"zz\"")
    assert(clue(ex.getMessage).contains("f"))

  test("malformed decimal literal throws IllegalArgumentException with arg name"):
    val ex = intercept[IllegalArgumentException]:
      arg("g", defaults.uint[16](16)).updateScalaValue("d\"16'hello\"")
    assert(clue(ex.getMessage).contains("g"))

end DesignArgSpec
