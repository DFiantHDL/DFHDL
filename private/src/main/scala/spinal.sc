import spinal.core._
import spinal.lib._

//Hardware definition
class MyTopLevel extends Component {
  val io = new Bundle {
    val (cond0, cond1) = (in  Bool, in  Bool)
    val flag  = out Bool
    val state = out UInt(8 bits)
  }
  val counter = Reg(UInt(8 bits)) init(0)

  when(io.cond0){
    counter := counter + 1
  }

  io.state := counter
  io.flag  := (counter === 0) | io.cond1
}

System.getProperty("user.dir")

SpinalVhdl(new MyTopLevel).rtlSourcesPaths



object Me {
  val (a, b) = (4, "irir")
}

//Me.getClass.getDeclaredFields
//
////Generate the MyTopLevel's VHDL
//object MyTopLevelVhdl {
//  def main(args: Array[String]) {
//    SpinalVhdl(new MyTopLevel)
//  }
//}
//
//

