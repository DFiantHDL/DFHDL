import DFiant._

object PCSel extends Enum.Manual(3) {
  val Plus4, Branch, Jump, JumpReg, Exception = Entry.incLastBy(1)
}
PCSel.Plus4.value
PCSel.Branch.value