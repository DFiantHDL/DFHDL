package DFiant

import internals._

abstract class DFFields()(implicit n : NameIt, ctx : DFFields.Context) extends DFAnyOwner {
  final def codeString : String = {
    s"\ntrait $name extends DFFields {$bodyCodeString\n}"
  }
}

object DFFields {
  type Context = DFAnyOwner.Context[DFAnyOwner]
}

