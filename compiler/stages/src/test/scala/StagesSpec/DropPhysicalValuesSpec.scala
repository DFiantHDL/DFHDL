package StagesSpec

import dfhdl.*
import dfhdl.compiler.stages.dropPhysicalValues
// scalafmt: { align.tokens = [{code = "<>"}, {code = "="}, {code = "=>"}, {code = ":="}]}

class DropPhysicalValuesSpec extends StageSpec:
  test("Basic physical value dropping"):
    class ID extends RTDesign:
      val x  = 5.MHz
      val y  = 2.Hz
      val y2 = 2.1.Hz
      val z  = (x / y).toInt
      val z2 = (x / y2).toDouble
    end ID
    val id = (new ID).dropPhysicalValues
    assertCodeString(
      id,
      """|class ID extends RTDesign:
         |  val z: Int <> CONST = 2500000
         |  val z2: Double <> CONST = 2380952.380952381
         |end ID""".stripMargin
    )
  test("Basic physical value dropping with CLK_FREQ at combinational only"):
    class ID extends RTDesign:
      val x  = CLK_FREQ
      val y  = 2.Hz
      val y2 = 2.1.Hz
      val z  = (x / y).toInt
      val z2 = (x / y2).toDouble
    end ID
    val id = (new ID).dropPhysicalValues
    assertCodeString(
      id,
      """|class ID extends RTDesign:
         |  val z: Int <> CONST = 0
         |  val z2: Double <> CONST = 0.0
         |end ID""".stripMargin
    )
  test("Basic physical value dropping with CLK_FREQ at combinational only"):
    class ID extends RTDesign:
      val i  = Int <> IN
      val x  = CLK_FREQ
      val y  = 2.Hz
      val y2 = 2.1.Hz
      val z  = (x / y).toInt
      val z2 = (x / y2).toDouble
      val o  = Int <> OUT
      o := i.reg(1, init = 0)
    end ID
    val id = (new ID).dropPhysicalValues
    assertCodeString(
      id,
      """|class ID extends RTDesign:
         |  val i = Int <> IN
         |  val z: Int <> CONST = 25000000
         |  val z2: Double <> CONST = 2.380952380952381E7
         |  val o = Int <> OUT
         |  o := i.reg(1, init = 0)
         |end ID""".stripMargin
    )

  test("Physical value dropping with multiple domains at different frequencies"):
    class MultiDomainDesign extends RTDesign:
      val i = Int <> IN
      val o = Int <> OUT

      // Main domain at 100MHz
      val mainFreq = CLK_FREQ
      val mainDiv  = (mainFreq / 10.Hz).toInt

      // Slow domain at 25MHz
      val slowClk    = ClkCfg(rate = 25.MHz)
      val slowRst    = RstCfg()
      val slowCfg    = RTDomainCfg(slowClk, slowRst)
      val slowDomain = new RTDomain(slowCfg):
        val slowFreq = CLK_FREQ
        val slowDiv  = (slowFreq / 5.Hz).toInt
        val slowReg  = Int <> VAR.REG init 0
        slowReg.din := i

      // Fast domain at 200MHz
      val fastClk    = ClkCfg(rate = 200.MHz)
      val fastRst    = RstCfg()
      val fastCfg    = RTDomainCfg(fastClk, fastRst)
      val fastDomain = new RTDomain(fastCfg):
        val fastFreq = CLK_FREQ
        val fastDiv  = (fastFreq / 50.Hz).toInt
        val fastReg  = Int <> VAR.REG init 0
        fastReg.din := slowDomain.slowReg

      o := fastDomain.fastReg.reg(1, init = 0)
    end MultiDomainDesign

    val design = (new MultiDomainDesign).dropPhysicalValues
    assertCodeString(
      design,
      """|class MultiDomainDesign extends RTDesign:
         |  val i = Int <> IN
         |  val o = Int <> OUT
         |  val mainDiv: Int <> CONST = 5000000
         |  val slowDomain = new RTDomain(slowCfg):
         |    val slowDiv: Int <> CONST = 5000000
         |    val slowReg = Int <> VAR.REG init 0
         |    slowReg.din := i
         |  end slowDomain
         |  val fastDomain = new RTDomain(fastCfg):
         |    val fastDiv: Int <> CONST = 4000000
         |    val fastReg = Int <> VAR.REG init 0
         |    fastReg.din := slowDomain.slowReg
         |  end fastDomain
         |  o := fastDomain.fastReg.reg(1, init = 0)
         |end MultiDomainDesign""".stripMargin
    )

  test("Physical value in wait statement is not dropped") {
    class WaitDesign extends RTDesign:
      val i      = Int <> IN
      val o      = Int <> OUT
      val period = 10.ms + 2.ms
      process:
        wait(period)
        wait(2.ms)
        o := i

    val design = (new WaitDesign).dropPhysicalValues
    assertCodeString(
      design,
      """|class WaitDesign extends RTDesign:
         |  val i = Int <> IN
         |  val o = Int <> OUT
         |  process:
         |    12.ms.wait
         |    2.ms.wait
         |    o := i
         |end WaitDesign""".stripMargin
    )
  }

  test("Regression check when data caching caused issues") {
    class Foo extends RTDesign:
      val cycles  = (CLK_FREQ * 3.sec).toInt
      val counter = Int <> OUT.REG init 0
      counter.din := counter + cycles
    val top = Foo().dropPhysicalValues
    assertCodeString(
      top,
      """|class Foo extends RTDesign:
         |  val cycles: Int <> CONST = 150000000
         |  val counter = Int <> OUT.REG init 0
         |  counter.din := counter + cycles
         |end Foo""".stripMargin
    )
  }
end DropPhysicalValuesSpec
