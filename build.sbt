name := "dfiant"
organization in ThisBuild := "hdl.dfiant"
scalaVersion in ThisBuild := "2.12.4-bin-typelevel-4"

version := "0.0.12-SNAPSHOT"

// PROJECTS

lazy val global = project
  .in(file("."))
  .settings(settings)
  .aggregate(
//    sourcecode,
//    `singleton-ops`,
//    continuum,
//    common,
    macros,
    core,
    examples
  )

//////////////////////////////////////////////////////////////////////////////////////
// Temporary replacements for libraries until all changes are merged into the masters
// * sourcecode
// * singleton-ops
// * continuum
//////////////////////////////////////////////////////////////////////////////////////
lazy val sourcecode = (project in file("modLibs/sourcecode"))
  .settings(
    name := "sourcecode",
    settings,
    macroSettings,
    assemblySettings,
    libraryDependencies ++= commonDependencies
  )

lazy val `singleton-ops` = (project in file("modLibs/singleton-ops"))
  .settings(
    name := "singleton-ops",
    settings,
    macroSettings,
    assemblySettings,
    libraryDependencies ++= commonDependencies ++ Seq(
      dependencies.macroCompat
    )
  )

lazy val continuum = (project in file("modLibs/continuum"))
  .settings(
    name := "continuum",
    settings,
    assemblySettings,
    libraryDependencies ++= commonDependencies ++ Seq(
      dependencies.scalacheck,
      dependencies.scalatest % "test"
    ),
    unmanagedSourceDirectories in Compile ++= {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, v)) if v >= 13 =>
          Seq(baseDirectory.value / "src" / "main" / "scala_2.13+")
        case _ =>
          Seq(baseDirectory.value / "src" / "main" / "scala_2.12-")
    }},
    unmanagedSourceDirectories in Test ++= {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, v)) if v >= 13 =>
          Seq(baseDirectory.value / "src" / "test" / "scala_2.13+")
        case _ =>
          Seq(baseDirectory.value / "src" / "test" / "scala_2.12-")
      }},
  )
//////////////////////////////////////////////////////////////////////////////////////

lazy val common = project
  .settings(settings)
  .dependsOn(
    sourcecode,
    `singleton-ops`,
    continuum
  )

lazy val macros = project
  .settings(
    name := "macros",
    settings,
    macroSettings,
    assemblySettings,
    libraryDependencies ++= commonDependencies
  )
  .dependsOn(
    common
  )

lazy val core = project
  .settings(
    name := "core",
    settings,
    assemblySettings,
    libraryDependencies ++= commonDependencies
  )
  .dependsOn(
    common
  )

//////////////////////////////////////////////////////////////////////////////////////
// Examples
//////////////////////////////////////////////////////////////////////////////////////
lazy val sorted_networks = (project in file("examples/sorted_networks"))
  .settings(
    name := "sorted_networks",
    settings,
    assemblySettings,
    libraryDependencies ++= commonDependencies
  )
  .dependsOn(
    core
  )

lazy val examples = project
  .settings(settings)
  .aggregate(
    sorted_networks
  )
//////////////////////////////////////////////////////////////////////////////////////


// DEPENDENCIES

lazy val dependencies =
  new {
    val logbackV        = "1.2.3"
    val logstashV       = "4.11"
    val scalaLoggingV   = "3.7.2"
    val typesafeConfigV = "1.3.1"
    val pureconfigV     = "0.8.0"
    val akkaV           = "2.5.19"
    val scalatestV      = "3.0.6-SNAP6"
    val scalacheckV     = "1.14.0"
    val singletonOpsV   = "0.3.2-SNAPSHOT"
    val shapelessV      = "2.3.3"
    val scodecV         = "1.1.9"
    val sourcecodeV     = "0.1.5-SNAPSHOT"
    val continuumV      = "0.4-SNAPSHOT"
    val macroParadiseV  = "2.1.1"
    val macroCompatV    = "1.1.1"

    val logback        = "ch.qos.logback"             % "logback-classic"          % logbackV
    val logstash       = "net.logstash.logback"       % "logstash-logback-encoder" % logstashV
    val scalaLogging   = "com.typesafe.scala-logging" %% "scala-logging"           % scalaLoggingV
    val typesafeConfig = "com.typesafe"               % "config"                   % typesafeConfigV
    val akka           = "com.typesafe.akka"          %% "akka-stream"             % akkaV
    val pureconfig     = "com.github.pureconfig"      %% "pureconfig"              % pureconfigV
    val scalatest      = "org.scalatest"              %% "scalatest"               % scalatestV
    val scalacheck     = "org.scalacheck"             %% "scalacheck"              % scalacheckV
    val singletonOps   = "eu.timepit"                 %% "singleton-ops"           % singletonOpsV
    val shapeless      = "com.chuusai"                %% "shapeless"               % shapelessV
    val scodec         = "org.scodec"                 %% "scodec-bits"             % scodecV
    val sourcecode     = "com.lihaoyi"                %% "sourcecode"              % sourcecodeV // Scala-JVM
    val continuum      = "danburkert"                 %% "continuum"               % continuumV
    val macroCompat    = "org.typelevel"              %% "macro-compat"            % macroCompatV
    val macroParadise  = compilerPlugin("org.scalamacros" % "paradise" % macroParadiseV cross CrossVersion.patch)
  }

lazy val commonDependencies = Seq(
  //  dependencies.singletonOps,
  //  dependencies.sourcecode,
  //  dependencies.continuum,
  dependencies.shapeless,
  dependencies.scodec,
  dependencies.akka,
  dependencies.scalacheck % "test"
)

// SETTINGS

lazy val settings =
  commonSettings //++ wartremoverSettings //++ scalafmtSettings

lazy val compilerOptions = Seq(
  "-unchecked",
  "-feature",
  "-language:reflectiveCalls",
  "-language:existentials",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-deprecation",
  "-encoding",
  "utf8"
)

lazy val macroSettings = Seq(
  scalacOptions ++= Seq(
    "-language:experimental.macros",
  ),
  scalacOptions ++= {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, v)) if v >= 13 =>
        Nil
      case _ =>
        Seq("-Xplugin-require:macroparadise")
    }
  },
  libraryDependencies ++= Seq(scalaOrganization.value % "scala-compiler" % scalaVersion.value),
  libraryDependencies ++= {
    CrossVersion.partialVersion(scalaVersion.value) match {
      // if scala 2.13+ is used, macro annotations are merged into scala-reflect
      // https://github.com/scala/scala/pull/6606
      case Some((2, v)) if v >= 13 =>
        Nil
      case _ =>
        Seq(dependencies.macroParadise)
    }
  }
)

lazy val commonSettings = Seq(
  scalaOrganization := {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, v)) if v >= 13 =>
        "org.scala-lang"
      case _ =>
        "org.typelevel"
    }
  },
  scalacOptions ++= compilerOptions,
  scalacOptions ++= {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, v)) if v >= 13 =>
        Nil
      case _ =>
        Seq(
          "-Yliteral-types", // enable SIP-23 implementation
          "-Xsource:2.13",   //https://github.com/scala/scala/commit/33478bdc9792ee13baa8208e326278695b1bd4e4
        )
    }
  },
  resolvers ++= Seq(
    "Local Maven Repository" at "file://" + Path.userHome.absolutePath + "/.m2/repository",
    Resolver.sonatypeRepo("releases"),
    Resolver.sonatypeRepo("snapshots")
  )
)

lazy val wartremoverSettings = Seq(
  wartremoverWarnings in (Compile, compile) ++= Warts.allBut(Wart.Throw)
)

lazy val scalafmtSettings =
  Seq(
    scalafmtOnCompile := true,
    scalafmtTestOnCompile := true,
    scalafmtVersion := "1.2.0"
  )

lazy val assemblySettings = Seq(
  assemblyJarName in assembly := name.value + ".jar",
  assemblyMergeStrategy in assembly := {
    case PathList("META-INF", xs @ _*) => MergeStrategy.discard
    case _                             => MergeStrategy.first
  }
)

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

//resolvers += "Oscar Snapshots" at "http://artifactory.info.ucl.ac.be/artifactory/libs-snapshot/"

//libraryDependencies += "oscar" %% "oscar-cp" % "4.1.0-SNAPSHOT"

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
//  "com.github.spinalhdl" % "spinalhdl-core_2.12" % "latest.release",
//  "com.github.spinalhdl" % "spinalhdl-lib_2.12" % "latest.release"
//)
////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////
// Treehugger (code generation)
////////////////////////////////////////////////////////////////////
//libraryDependencies += "com.eed3si9n" %% "treehugger" % "0.4.1"
//
//resolvers += Resolver.sonatypeRepo("public")
////////////////////////////////////////////////////////////////////
