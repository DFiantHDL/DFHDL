// format: off
val projectName = "dfiant"
val compilerVersion = "3.1.1-RC2"

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
    lib
  )

lazy val plugin = project
  .settings(
    name := s"$projectName-plugin",
    settings,
    crossTarget := target.value / s"scala-${scalaVersion.value}", // workaround for https://github.com/sbt/sbt/issues/5097
    crossVersion := CrossVersion.full,
    libraryDependencies += "org.scala-lang" %% "scala3-compiler" % compilerVersion % "provided"
  ).dependsOn(internals)

lazy val internals = project
  .settings(
    name := s"$projectName-internals",
    settings,
    libraryDependencies ++= commonDependencies
  )

lazy val compiler_ir = project
  .settings(
    name := s"$projectName-compiler-ir",
    settings
  ).dependsOn(internals)

lazy val core = project
  .settings(
    name := s"$projectName-core",
    settings,
    pluginTestUseSettings,
    libraryDependencies ++= commonDependencies :+ dependencies.scalafmt
  )
  .dependsOn(
    plugin,
    internals,
    compiler_ir
  )

lazy val lib = project
  .settings(
    name := projectName,
    settings,
    pluginUseSettings,
    libraryDependencies ++= commonDependencies
  )
  .dependsOn(
    core % "test->test;compile->compile"
  )

// DEPENDENCIES

lazy val dependencies =
  new {
    private val scodecV = "1.1.30"
    private val munitV = "0.7.29"
    private val scalafmtV = "3.3.1"

    val scodec = "org.scodec" %% "scodec-bits" % scodecV
    val munit = "org.scalameta" %% "munit" % munitV % Test
    val scalafmt = ("org.scalameta" %% "scalafmt-dynamic" % scalafmtV).cross(CrossVersion.for3Use2_13)
  }

lazy val commonDependencies = Seq(
  dependencies.scodec,
  dependencies.munit
)

// SETTINGS

lazy val settings =
  commonSettings

lazy val compilerOptions = Seq(
  "-unchecked",
  "-feature",
  "-language:strictEquality",
  "-language:implicitConversions",
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
