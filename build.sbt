commands += DFHDLCommands.quickTestSetup
commands += DFHDLCommands.docExamplesRefUpdate

// format: off
val projectName = "dfhdl"
val compilerVersion = "3.6.3"

inThisBuild(
  List(
    homepage := Some(url("https://dfianthdl.github.io/")),
    licenses := List(
      "LGPL" -> url("https://www.gnu.org/licenses/lgpl-3.0.txt")
    ),
    developers := List(
      Developer(
        "soronpo",
        "Oron Port",
        "",
        url("https://twitter.com/soronpo")
      )
    )
  )
)

name := projectName
ThisBuild / organization := "io.github.dfianthdl"
ThisBuild / scalaVersion := compilerVersion
ThisBuild / versionScheme := Some("semver-spec")
//ThisBuild / version      := "0.3.0-SNAPSHOT"


// PROJECTS
lazy val root = (project in file("."))
  .settings(
    settings,
    publish / skip := true
  )
  .aggregate(
    internals,
    plugin,
    compiler_ir,
    core,
	  compiler_stages,
    lib
  )

lazy val internals = project
  .settings(
    name := s"$projectName-internals",
    settings,
    libraryDependencies ++= commonDependencies
  )

def additionalSources(scalaVersion: String, base: File): Seq[File] = {
  CrossVersion.partialVersion(scalaVersion) match {
    case Some((3, minor)) if minor <= 4 =>
      Seq(base / "src" / "main" / "scala-3.4-")
    case Some((3, minor)) if minor >= 5 =>
      Seq(base / "src" / "main" / "scala-3.5+")
    case _ =>
      Seq.empty
  }
}
lazy val plugin = project
  .settings(
    name := s"$projectName-plugin",
    settings,
    crossTarget := target.value / s"scala-${scalaVersion.value}", // workaround for https://github.com/sbt/sbt/issues/5097
    crossVersion := CrossVersion.full,
    Compile / unmanagedSourceDirectories ++= {
      val base = baseDirectory.value
      additionalSources(scalaVersion.value, base)
    },
    libraryDependencies += "org.scala-lang" %% "scala3-compiler" % compilerVersion % "provided"
  ).dependsOn(internals)

lazy val compiler_ir = (project in file("compiler/ir"))
  .settings(
    name := s"$projectName-compiler-ir",
    settings
  ).dependsOn(internals)

lazy val core = project
  .settings(
    name := s"$projectName-core",
    settings,
    pluginTestUseSettings,
    libraryDependencies ++= commonDependencies :+ dependencies.scalafmt,
    Compile / resourceGenerators += Def.task {
      val file = (Compile / resourceManaged).value / "version.properties"
      val contents = s"version=${version.value}"
      IO.write(file, contents)
      Seq(file)
    }.taskValue
  )
  .dependsOn(
    plugin,
    internals,
    compiler_ir
  )

lazy val compiler_stages = (project in file("compiler/stages"))
  .settings(
    name := s"$projectName-compiler-stages",
    settings,
    pluginTestUseSettings,
    libraryDependencies ++= commonDependencies
  )
  .dependsOn(
    plugin,
    internals,
    compiler_ir,
    core
  )

lazy val lib = project
  .settings(
    name := projectName,
    settings,
    pluginUseSettings,
    libraryDependencies ++= commonDependencies
  )
  .dependsOn(
    core % "test->test;compile->compile",
    compiler_stages
  )

// DEPENDENCIES

lazy val dependencies =
  new {
    private val scodecV = "1.2.1"
    private val munitV = "1.1.0"
    private val scalafmtV = "3.8.3"
    private val airframelogV = "2025.1.8"
    private val oslibV = "0.9.2"
    private val scallopV = "5.2.0"
    val scodec = "org.scodec" %% "scodec-bits" % scodecV
    val munit = "org.scalameta" %% "munit" % munitV % Test
    val scalafmt = ("org.scalameta" %% "scalafmt-dynamic" % scalafmtV).cross(CrossVersion.for3Use2_13)
    val airframelog = "org.wvlet.airframe" %% "airframe-log" % airframelogV
    val oslib = "com.lihaoyi" %% "os-lib" % oslibV
    val scallop = "org.rogach" %% "scallop" % scallopV
  }

lazy val commonDependencies = Seq(
  dependencies.scodec,
  dependencies.munit,
  dependencies.airframelog,
  dependencies.scallop
)

// SETTINGS

lazy val settings =
  commonSettings

def compilerOptionsVersionDependent(scalaVersion: String) = {
  CrossVersion.partialVersion(scalaVersion) match {
    case Some((3, minor)) if minor <= 4 =>
      Seq.empty
    case Some((3, minor)) if minor >= 5 =>
      Seq.empty
    case _ =>
      Seq.empty
  }
}

lazy val compilerOptions = Seq(
  "-unchecked",
  "-feature",
  "-language:strictEquality",
  "-language:implicitConversions",
  "-deprecation",
  //TODO: remove when fixed scalac issues:
  //https://github.com/lampepfl/dotty/issues/19299
  "-Wconf:msg=or backticked identifier `equals`:s",
  //https://github.com/lampepfl/dotty/issues/19301
  "-Wconf:msg=not declared infix:s",
  //ignore warning given by the plugin Jdummy dependency trick
  "-Wconf:msg=bad option '-Jdummy:s"
)

lazy val pluginUseSettings = Seq(
  Compile / scalacOptions ++= {
    val jar = (plugin / Compile / packageBin).value
    Seq(
      s"-Xplugin:${jar.getAbsolutePath}",
      s"-Jdummy=${jar.lastModified}"
    )
  }
)

lazy val pluginTestUseSettings = Seq(
  Test / scalacOptions ++= {
    val jar = (plugin / Compile / packageBin).value
    Seq(
      s"-Xplugin:${jar.getAbsolutePath}",
      s"-Jdummy=${jar.lastModified}"
    )
  }
)

lazy val commonSettings = Seq(
  scalacOptions ++= {
    compilerOptions ++ compilerOptionsVersionDependent(scalaVersion.value)
  }
)
