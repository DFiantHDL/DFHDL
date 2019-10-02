package DFiant
import internals._

trait DFApp extends App {
  final protected implicit val __allowTop = DFDesign.allowTop.__AllowTop
}

object DFApp {
  abstract class VHDLCompiler[DFD <: DFDesign](path : String = "./")(
    implicit gen : DFDesign.Gen[DFD]
  ) extends DFApp {
    val top : DFD = gen() //Instantiate a top-level instance
    top.setAutoName(top.typeName).compileToVHDL.toFolder(path) //Compile to VHDL and write the files to path.
  }
}