val projectName = "dfiant"

inThisBuild(List(
  homepage     := Some(url("https://dfianthdl.github.io/")),
  licenses     := List("LGPL" -> url("https://www.gnu.org/licenses/lgpl-3.0.txt")),
  developers := List(
    Developer(
      "soronpo",
      "Oron Port",
      "",
      url("https://www.researchgate.net/profile/Oron_Port")
    )
)))

name := projectName
organization in ThisBuild := "io.github.dfianthdl"
scalaVersion in ThisBuild := "2.13.5"
//resolvers in ThisBuild += "pr" at "https://scala-ci.typesafe.com/artifactory/scala-pr-validation-snapshots/"
resolvers in ThisBuild += "scala-integration" at
  "https://scala-ci.typesafe.com/artifactory/scala-integration/"

//resolvers in ThisBuild += "Oscar Snapshots" at "http://artifactory.info.ucl.ac.be/artifactory/libs-snapshot/"

//version in ThisBuild := "0.0.20-SNAPSHOT"
//enablePlugins(ScalaJSPlugin)

// PROJECTS

lazy val root = project
  .in(file("."))
  .settings(settings)
  .aggregate(
    continuum,
    internals,
    core,
    lib
  )

//////////////////////////////////////////////////////////////////////////////////////
// Temporary replacements for libraries until all changes are merged into the masters
// * continuum
//////////////////////////////////////////////////////////////////////////////////////
lazy val continuum = (project in file("modLibs/continuum"))
  .settings(
    name := "continuum",
    settings,
    assemblySettings,
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

lazy val internals = project
  .settings(
    name := s"$projectName-internals",
    settings,
    macroSettings,
    assemblySettings,
    libraryDependencies ++= commonDependencies
  )
  .dependsOn(
    continuum
  )

lazy val core = project
  .settings(
    name := s"$projectName-core",
    settings,
    macroSettings,
    assemblySettings,
    libraryDependencies ++= commonDependencies
  )
  .dependsOn(
    internals
  )

lazy val lib = project
  .settings(
    name := projectName,
    settings,
    macroSettings,
    assemblySettings,
    libraryDependencies ++= commonDependencies
  )
  .dependsOn(
    core % "test->test;compile->compile"
  )

//////////////////////////////////////////////////////////////////////////////////////
// Examples
//////////////////////////////////////////////////////////////////////////////////////
lazy val `first-look` = (project in file("examples/first-look"))
  .settings(
    name := "first-look",
    skip in publish := true,
    settings,
    assemblySettings,
    libraryDependencies ++= commonDependencies
  )
  .dependsOn(
    core,
    lib
  )

lazy val examples = project
  .settings(
    skip in publish := true,
    settings
  )
  .aggregate(
    `first-look`
  )
//////////////////////////////////////////////////////////////////////////////////////


// DEPENDENCIES

lazy val dependencies =
  new {
    private val logbackV        = "1.2.3"
    private val logstashV       = "4.11"
    private val scalaLoggingV   = "3.7.2"
    private val typesafeConfigV = "1.3.1"
    private val pureconfigV     = "0.8.0"
    private val akkaV           = "2.6.14"
    private val singletonOpsV   = "0.5.2"
    private val shapelessV      = "2.3.4"
    private val scodecV         = "1.1.12"
    private val oslibV          = "0.7.4"
    private val continuumV      = "0.4-SNAPSHOT"
    private val macroParadiseV  = "2.1.1"
    private val macroCompatV    = "1.1.1"
    private val ammoniteV       = "2.3.8"
    private val oscarV          = "4.1.0-SNAPSHOT"
    private val munitV          = "0.7.22"

    val logback        = "ch.qos.logback"             % "logback-classic"          % logbackV
    val logstash       = "net.logstash.logback"       % "logstash-logback-encoder" % logstashV
    val scalaLogging   = "com.typesafe.scala-logging" %% "scala-logging"           % scalaLoggingV
    val typesafeConfig = "com.typesafe"               % "config"                   % typesafeConfigV
    val akka           = "com.typesafe.akka"          %% "akka-stream"             % akkaV
    val pureconfig     = "com.github.pureconfig"      %% "pureconfig"              % pureconfigV
    val singletonOps   = "eu.timepit"                 %% "singleton-ops"           % singletonOpsV
    val shapeless      = "com.chuusai"                %% "shapeless"               % shapelessV
    val scodec         = "org.scodec"                 %% "scodec-bits"             % scodecV
    val oslib          = "com.lihaoyi"                %% "os-lib"                  % oslibV
    val continuum      = "danburkert"                 %% "continuum"               % continuumV
    val macroCompat    = "org.typelevel"              %% "macro-compat"            % macroCompatV
    val ammoniteOps    = "com.lihaoyi"                %% "ammonite-ops"            % ammoniteV
    val ammonite       = "com.lihaoyi"                %  "ammonite"                % ammoniteV % "test" cross CrossVersion.full
    val oscar          = "oscar"                      %% "oscar-cp"                % oscarV
    val munit          = "org.scalameta"              %% "munit"                   % munitV % Test
    val macroParadise  = compilerPlugin("org.scalamacros" % "paradise" % macroParadiseV cross CrossVersion.patch)
  }

lazy val commonDependencies = Seq(
  dependencies.singletonOps,
  dependencies.oslib,
  //  dependencies.continuum,
  dependencies.shapeless,
  dependencies.scodec,
  dependencies.akka,
//  dependencies.oscar,
  dependencies.ammoniteOps,
//  dependencies.ammonite,
  dependencies.munit
)

// SETTINGS

lazy val settings =
  commonSettings 

lazy val compilerOptions = Seq(
  "-unchecked",
  "-feature",
  "-language:reflectiveCalls",
  "-language:existentials",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-deprecation",
//  "-Vimplicits",
  "-encoding",
  "utf8"
)

lazy val macroSettings = Seq(
  scalacOptions ++= Seq(
    "-language:experimental.macros",
    "-Ymacro-annotations"
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
  scalacOptions ++= Seq(
    "-language:experimental.macros",
    "-Ymacro-annotations"
  ),
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
  ),
//  libraryDependencies += dependencies.ammonite,
//  sourceGenerators in Test += Def.task {
//    val file = (sourceManaged in Test).value / "amm.scala"
//    IO.write(file, """object amm extends App { ammonite.Main.main(args) }""")
//    Seq(file)
//  }.taskValue
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
