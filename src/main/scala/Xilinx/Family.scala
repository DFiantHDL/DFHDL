package Xilinx

sealed trait Family {

}
object Family {
  trait Artix7 extends Family with Series.`7`
  trait Artix7LowVoltage extends Family with Series.`7`
  trait Kintex7 extends Family with Series.`7`
  trait Virtex7 extends Family with Series.`7`
  trait Kintex7LowVoltage extends Family with Series.`7`
  trait KintexUltraScale extends Family with Series.`7`
  trait XAArtix7 extends Family with Series.`7`
  trait XAZync7000 extends Family with Series.`7`
  trait Zync7000 extends Family with Series.`7`
}
