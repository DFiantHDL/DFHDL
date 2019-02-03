//package DFiant
//
//import internals._
//
//abstract class DFFields()(implicit ctx0 : DFFields.Context) extends DFAnyOwner {
//  import __dev._
//  final lazy val ctx = ctx0
//  final def codeString : String = {
//    s"\ntrait $name extends DFFields {$bodyCodeString\n}"
//  }
//  final val id = getID
//}
//
//object DFFields {
//  type Context = DFAnyOwner.Context[DFAnyOwner]
//}
//
