commands += DFHDLCommands.libPlayground
commands += DFHDLCommands.corePlayground
commands += DFHDLCommands.clearSandbox
commands += DFHDLCommands.testApps
commands += DFHDLCommands.docExamplesRefUpdate

// format: off
val projectName = "dfhdl"

// VERSIONS — all version literals live here.
val compilerVersion = "3.8.4"
// The DFTools binary toolchain release this DFHDL build targets (versioned independently of
// DFHDL). Surfaced to the library via version.properties and read by DFToolsImage. Bump when
// adopting a new DFTools release.
val dftoolsVersion = "v0.2.0"
// dependency versions
val scodecVersion        = "1.2.5"
val munitVersion         = "1.3.3"
val airframelogVersion   = "2026.1.6"
val oslibVersion         = "0.11.8"
val scallopVersion       = "6.0.0"
val upickleVersion       = "4.4.3"
val scalapptainerVersion = "0.5.1"

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
    ),
    publishTo := {
      val centralSnapshots = "https://central.sonatype.com/repository/maven-snapshots/"
      if (isSnapshot.value) Some("central-snapshots" at centralSnapshots)
      else localStaging.value
    }
  )
)

name := projectName
ThisBuild / organization := "io.github.dfianthdl"
ThisBuild / scalaVersion := compilerVersion
ThisBuild / versionScheme := Some("semver-spec")
ThisBuild / resolvers += Resolver.scalaNightlyRepository
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
    lib,
    platforms
  )

lazy val internals = project
  .settings(
    name := s"$projectName-internals",
    settings,
    implicitConversionSettings,
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
    settings,
    implicitConversionSettings,
    libraryDependencies += dependencies.upickle
  ).dependsOn(internals)

lazy val core = project
  .settings(
    name := s"$projectName-core",
    settings,
    implicitConversionSettings,
    pluginTestUseSettings,
    libraryDependencies ++= commonDependencies,
    Compile / resourceGenerators += Def.task {
      val file = (Compile / resourceManaged).value / "version.properties"
      val contents = s"version=${version.value}\ndftools.version=$dftoolsVersion"
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
    libraryDependencies ++= commonDependencies,
    libraryDependencies += dependencies.scalapptainer
  )
  .dependsOn(
    core % "test->test;compile->compile",
    compiler_stages
  )

lazy val platforms = project
  .settings(
    name := s"$projectName-platforms",
    settings,
    // Override global license: platforms module is published under Apache 2.0
    licenses := List(
      "Apache-2.0" -> url("https://www.apache.org/licenses/LICENSE-2.0.txt")
    ),
    pluginUseSettings,
    libraryDependencies ++= commonDependencies
  )
  .dependsOn(
    core % "test->test",
    lib
  )

// DEPENDENCIES

lazy val dependencies =
  new {
    val scodec = "org.scodec" %% "scodec-bits" % scodecVersion
    val munit = "org.scalameta" %% "munit" % munitVersion % Test
    val airframelog = "org.wvlet.airframe" %% "airframe-log" % airframelogVersion
    val oslib = "com.lihaoyi" %% "os-lib" % oslibVersion
    val scallop = "org.rogach" %% "scallop" % scallopVersion
    val upickle = "com.lihaoyi" %% "upickle" % upickleVersion
    // Scalapptainer: cross-platform Apptainer wrapper used to run the DFTools image
    val scalapptainer = "io.github.dfiantworks" %% "scalapptainer" % scalapptainerVersion
  }

lazy val commonDependencies = Seq(
  dependencies.scodec,
  dependencies.munit,
  dependencies.airframelog,
  dependencies.scallop,
  dependencies.oslib
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
  "-preview",
  "-language:strictEquality",
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
      s"-Jdummy=${jar.lastModified}",
      // "-Yprofile-enabled",
      // "-Yprofile-trace:compiler.trace"
    )
  }
)

lazy val commonSettings = Seq(
  scalacOptions ++= {
    compilerOptions ++ compilerOptionsVersionDependent(scalaVersion.value)
  }
)

lazy val implicitConversionSettings = Seq(
  Compile / scalacOptions ++= Seq(
    "-language:implicitConversions"
  )
)
