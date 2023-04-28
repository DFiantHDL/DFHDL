// format: off
val projectName = "dfhdl"
val compilerVersion = "3.3.0-RC4"

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
        url("https://www.researchgate.net/profile/Oron_Port")
      )
    )
  )
)

name := projectName
ThisBuild / organization := "io.github.dfianthdl"
ThisBuild / scalaVersion := compilerVersion
ThisBuild / version      := "0.2.17-SNAPSHOT"


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

lazy val plugin = project
  .settings(
    name := s"$projectName-plugin",
    settings,
    crossTarget := target.value / s"scala-${scalaVersion.value}", // workaround for https://github.com/sbt/sbt/issues/5097
    crossVersion := CrossVersion.full,
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
    private val scodecV = "1.1.34"
    private val munitV = "0.7.29"
    private val scalafmtV = "3.3.1"
    private val airframelogV = "22.7.3"
    val scodec = "org.scodec" %% "scodec-bits" % scodecV
    val munit = "org.scalameta" %% "munit" % munitV % Test
    val scalafmt = ("org.scalameta" %% "scalafmt-dynamic" % scalafmtV).cross(CrossVersion.for3Use2_13)
    val airframelog = "org.wvlet.airframe" %% "airframe-log" % airframelogV
  }

lazy val commonDependencies = Seq(
  dependencies.scodec,
  dependencies.munit,
  dependencies.airframelog
)

// SETTINGS

lazy val settings =
  commonSettings

lazy val compilerOptions = Seq(
  "-unchecked",
  "-feature",
  "-language:strictEquality",
  "-language:implicitConversions",
  "-language:experimental",
  "-deprecation",
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
  scalacOptions ++= compilerOptions
)
