package Xilinx

trait Package {
  val pinCount : Int with Singleton
}

object Package {
  trait FFG1761 extends Package {
    final val pinCount : Int with Singleton = 1761
  }

}
