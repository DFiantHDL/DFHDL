package Xilinx

trait Device {
  trait Resources {
    val Slices : Int with Singleton
    val LogicCells : Int with Singleton
    val FlipFlops : Int with Singleton
    val BlockRAMs36Kb : Int with Singleton
    val CMTs : Int with Singleton
    val DSPs : Int with Singleton
    val GTPE2Trans : Int with Singleton
    val GbTrans : Int with Singleton
    val IOBs : Int with Singleton
  }
  val resources : Resources = ???

}

object Device {
  trait XC7K70T extends Device with Family.Kintex7 {
  }
  trait XC7K160T extends Device with Family.Kintex7 {
  }
  trait XC7K325T extends Device with Family.Kintex7 {
  }
  trait XC7K355T extends Device with Family.Kintex7 {
  }
  trait XC7K410T extends Device with Family.Kintex7 {
  }
  trait XC7K420T extends Device with Family.Kintex7 {
  }
  trait XC7K480T extends Device with Family.Kintex7 {
  }

  trait XC7V585T extends Device with Family.Virtex7 {
  }
  trait XC7V2000T extends Device with Family.Virtex7 {
  }
  trait XC7VX330T extends Device with Family.Virtex7 {
  }
  trait XC7VX415T extends Device with Family.Virtex7 {
  }
  trait XC7VX485T extends Device with Family.Virtex7 {
  }
  trait XC7VX550T extends Device with Family.Virtex7 {
  }
  trait XC7VX690T extends Device with Family.Virtex7 {
  }
  trait XC7VX980T extends Device with Family.Virtex7 {
  }
  trait XC7VX1140T extends Device with Family.Virtex7 {
  }
  trait XC7VH580T extends Device with Family.Virtex7 {
  }
  trait XC7VH870T extends Device with Family.Virtex7 {
  }
}
