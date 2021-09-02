val projectName = "dfiant"
val compilerVersion = "3.1.0-RC1"

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

lazy val root = project
  .in(file("."))
  .settings(
    settings,
    publish / skip := true
  )
  .aggregate(
    plugin,
    internals,
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
  )

lazy val internals = project
  .settings(
    name := s"$projectName-internals",
    settings,
    libraryDependencies ++= commonDependencies
  )

lazy val core = project
  .settings(
    name := s"$projectName-core",
    settings,
    pluginTestUseSettings,
    libraryDependencies ++= commonDependencies
  )
  .dependsOn(
    plugin,
    internals
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
    private val scodecV = "1.1.27"
    private val munitV = "0.7.26"

    val scodec = "org.scodec" %% "scodec-bits" % scodecV
    val munit = "org.scalameta" %% "munit" % munitV % Test
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
  "-language:implicitConversions",
  "-deprecation"
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
