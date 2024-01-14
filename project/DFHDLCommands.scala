import sbt._
import Keys._

object DFHDLCommands {
  val quickTestSetup = Command.command("quickTestSetup") { state =>
    val extracted = Project.extract(state)
    val newState = extracted.appendWithSession(Seq(
      (LocalProject("internals") / Test / sources) := Nil,
      (LocalProject("core") / Test / sources) := Nil,
      (LocalProject("compiler_stages") / Test / sources) := Nil,
      (LocalProject("lib") / Test / sources) := ((LocalProject("lib") / Test / sources).value.filter(_.toString.contains("Example.scala")))
    ), state)
    newState
  }
}