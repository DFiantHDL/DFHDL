package DFiant
import internals._

trait DFApp extends App {
  final protected implicit val __allowTop = DFDesign.allowTop.__AllowTop
}

object DFApp {
  abstract class VHDLCompiler[DFD <: DFDesign](config: Config = Config.ToFolder())(
    implicit gen : DFDesign.Gen[DFD]
  ) extends DFApp {
    val top : DFD = gen() //Instantiate a top-level instance
    val topVHDL = top.setAutoName(top.typeName).compileToVHDL
    config match {
      case Config.Print => topVHDL.print()
      case Config.ToFolder(path) => topVHDL.toFolder(path)
    }
  }
  sealed trait Config extends Product with Serializable
  object Config {
    case object Print extends Config
    final case class ToFolder(path : String = "./") extends Config
  }
}