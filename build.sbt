name := "DFiant"

version := "1.0"

scalaVersion := "2.12.4-bin-typelevel-4"

//scalacOptions += "-Ypartial-unification" // enable fix for SI-2712
scalacOptions += "-deprecation"

scalacOptions += "-Yliteral-types"        // enable SIP-23 implementation

//scalacOptions += "-Ykind-polymorphism"    //enable Kind polymorphism

//enablePlugins(DottyPlugin)

scalacOptions ++= Seq("-feature")

scalacOptions ++= Seq("-language:reflectiveCalls")

scalacOptions ++= Seq("-language:existentials")

scalacOptions ++= Seq("-language:implicitConversions")

scalacOptions ++= Seq("-language:higherKinds")

parallelExecution in Test := false


////////////////////////////////////////////////////////////////////
// Singleton-ops
////////////////////////////////////////////////////////////////////
libraryDependencies ++= Seq(
  "eu.timepit" %% "singleton-ops" % "0.3.0-SNAPSHOT"
)
////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////
// Shapeless
////////////////////////////////////////////////////////////////////
resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots")
)

libraryDependencies ++= Seq(
  "com.chuusai" %% "shapeless" % "2.3.2"
)
//////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////
// scodec
////////////////////////////////////////////////////////////////////
libraryDependencies += "org.scodec" %% "scodec-bits" % "1.1.5"
////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////
// Refined (type safety)
////////////////////////////////////////////////////////////////////
//libraryDependencies ++= Seq(
//  "eu.timepit" %% "refined"            % "0.7.0",
////  "eu.timepit" %% "refined-scalaz"     % "0.5.0",         // optional
////  "eu.timepit" %% "refined-scodec"     % "0.5.0",         // optional
//  "eu.timepit" %% "refined-scalacheck" % "0.7.0" % "test" // optional
//)
////////////////////////////////////////////////////////////////////



////////////////////////////////////////////////////////////////////
// Spire
////////////////////////////////////////////////////////////////////
//libraryDependencies += "org.spire-math" %% "spire" % "0.11.0"
////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////
// Tagging
////////////////////////////////////////////////////////////////////
//libraryDependencies += "com.softwaremill.common" %% "tagging" % "1.0.0"
////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////
// Oscar (constraints)
////////////////////////////////////////////////////////////////////
//resolvers += "Oscar Snapshots" at "http://artifactory.info.ucl.ac.be/artifactory/libs-release/"
//
//libraryDependencies += "oscar" %% "oscar-cp" % "3.0.0"
//
//libraryDependencies += "oscar" %% "oscar-linprog" % "3.0.0"
//
//libraryDependencies += "oscar" %% "oscar-cbls" % "3.0.0"
//
//libraryDependencies += "oscar" %% "oscar-dfo" % "3.0.0"
////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////
// Configs (configuration)
////////////////////////////////////////////////////////////////////
//libraryDependencies += "com.github.kxbmap" %% "configs" % "0.4.2"
////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////
// Scalacheck
////////////////////////////////////////////////////////////////////
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.13.4" % "test"
////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////
// Scalatest
////////////////////////////////////////////////////////////////////
//libraryDependencies += "org.scalatest" % "scalatest_2.11" % "latest.release"
////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////
// Chisel
////////////////////////////////////////////////////////////////////
//libraryDependencies += "edu.berkeley.cs" %% "chisel" % "latest.release"
////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////
// SpinalHDL
////////////////////////////////////////////////////////////////////
//libraryDependencies ++= Seq(
//  "com.github.spinalhdl" % "spinalhdl-core_2.11" % "latest.release",
//  "com.github.spinalhdl" % "spinalhdl-lib_2.11" % "latest.release"
//)
////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////
// Treehugger (code generation)
////////////////////////////////////////////////////////////////////
//libraryDependencies += "com.eed3si9n" %% "treehugger" % "0.4.1"
//
//resolvers += Resolver.sonatypeRepo("public")
////////////////////////////////////////////////////////////////////




//libraryDependencies += "com.lihaoyi" % "ammonite-repl" % "0.6.2" % "test" cross CrossVersion.full
//
initialCommands in console :=
  """
    |import DFiant._
    |
    |""".stripMargin
//
