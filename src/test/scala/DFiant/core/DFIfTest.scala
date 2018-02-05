package DFiant.core

import DFiant._

object IfTest {
  implicit val dsn = GlobalDesign
  val a = DFBool()

  ifdf (a) {

  } elseifdf (a || a) {

  }
}
