package Xilinx

sealed trait TempGrade {

}

object TempGrade {
  trait C extends TempGrade
  trait E extends TempGrade
  trait I extends TempGrade
  trait Q extends TempGrade
}
