package DFiant.core

import DFiant._

object IfTest {
  import GlobalDesign._
  val a = DFBool()

  ifdf (a) {

  } elseifdf (a || a) {

  }
}
