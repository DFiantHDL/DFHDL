package DFiant
import internals._

trait DFApp extends App {
  final protected implicit val __AllowTop = DFDesign.allowTop.__AllowTop
}

object DFApp {
  abstract class VHDLCompiler[DFD <: DFDesign](path : String = "./")(
    implicit gen : DFDesign.Gen[DFD]
  ) extends DFApp {
    val top = gen() //Instantiate a top-level instance
    top.setAutoName(top.typeName).compileToVHDL.toFolder(path) //Compile to VHDL and write the files to path.
  }
}