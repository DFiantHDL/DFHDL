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
// DFHDL). Surfaced to the library via lib's generated `dftools.properties` and read by
// DFToolsImage. Bump when adopting a new DFTools release.
val dftoolsVersion = "v0.2.0"
// The vga-monitor-sim release wrapped by the `dfhdl.ips.video.vga.vga_monitor` foreign IP. This is
// the single source of truth: it is surfaced to the IP code via the generated `vga-monitor.properties`
// resource (read by `vga_monitor.version`), like core's version.properties. Since v0.3.0 the release names
// all files unversioned (the version lives only in the archive/folder name), so the IP's per-FFI lib
// base names are unversioned too. v0.4.0 adds the self-describing `VGA_MONITOR_FORMAT=ppm` stream
// (per-frame P6 header carrying width/height); the viewer opts into it to auto-size frames.
val vgaMonitorVersion = "0.4.0"
// dependency versions
val scodecVersion        = "1.2.5"
val munitVersion         = "1.3.3"
val airframelogVersion   = "2026.1.7"
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
    platforms,
    ips
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
      val contents = s"version=${version.value}"
      // Only (re)write when the content actually changes. An unconditional `IO.write` bumps the
      // file mtime every build, forcing `copyResources` to re-copy it to `classes/` each run; on
      // Windows that rename intermittently fails with AccessDenied if any reader still holds the
      // file open. Preserving mtime when unchanged avoids the needless re-copy entirely.
      if (!file.exists || IO.read(file) != contents) IO.write(file, contents)
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
    libraryDependencies += dependencies.scalapptainer,
    // The DFTools toolchain version is owned by `lib` (where `DFToolsImage` reads it) rather than
    // shared with core's `version.properties`. Conditional write to avoid mtime churn (see core).
    Compile / resourceGenerators += Def.task {
      val file = (Compile / resourceManaged).value / "dftools.properties"
      val contents = s"dftools.version=$dftoolsVersion"
      if (!file.exists || IO.read(file) != contents) IO.write(file, contents)
      Seq(file)
    }.taskValue
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

lazy val ips = project
  .settings(
    name := s"$projectName-ips",
    settings,
    pluginUseSettings,
    libraryDependencies ++= commonDependencies,
    // Surface the wrapped vga-monitor-sim version to the IP code (read by
    // `dfhdl.ips.video.vga.vga_monitor.version`), mirroring core's `version.properties` and lib's
    // `dftools.properties`. Conditional write to avoid mtime churn (see core's note: an unconditional
    // IO.write forces a re-copy each build, which intermittently fails with AccessDenied on Windows).
    Compile / resourceGenerators += Def.task {
      val file = (Compile / resourceManaged).value / "vga-monitor.properties"
      val contents = s"vga-monitor.version=$vgaMonitorVersion"
      if (!file.exists || IO.read(file) != contents) IO.write(file, contents)
      Seq(file)
    }.taskValue,
    // Download the vga-monitor-sim release binaries + HDL wrappers and bundle them as resources of
    // the `vga_monitor` foreign IP. The HDL wrappers (identical across platforms) live directly in
    // the IP folder root; per-system binaries go under `<platform>/` (selected at simulate time, and
    // kept in per-platform subfolders because v0.3.0's unversioned filenames collide across
    // platforms — e.g. `libvga_monitor_dpi.so` in both linux-x86_64 and linux-arm64). Since v0.3.0
    // the release names everything unversioned (the version is only in the archive/folder name), so
    // files are copied as-is — HDL wrappers (`.sv`/`.v`/`.vhdl`) by extension, everything else (libs,
    // import libs, `.vpi`, `.lib`/`.exp`, ...) as a per-system binary. Downloads are cached.
    Compile / resourceGenerators += Def.task {
      val log = streams.value.log
      val ver = vgaMonitorVersion
      val repo = "DFiantWorks/vga-monitor-sim"
      val platforms = Seq(
        "linux-x86_64", "linux-arm64", "macos-x86_64", "macos-arm64",
        "windows-x86_64", "windows-x86_64-mingw"
      )
      // under a non-package root (`dfhdl-ips`) so the resource dir is not read as a Scala package
      val baseRes = (Compile / resourceManaged).value / "dfhdl-ips" / "vga_monitor"
      val cacheDir = target.value / "vga-monitor-cache"
      IO.createDirectory(cacheDir)
      def isHdl(n: String): Boolean =
        n.endsWith(".sv") || n.endsWith(".v") || n.endsWith(".vhdl")
      val generated = scala.collection.mutable.ListBuffer.empty[File]
      var hdlCopied = false
      platforms.foreach { plat =>
        val asset = s"vga-monitor-$ver-$plat.tar.gz"
        val tarball = cacheDir / asset
        val srcDir = cacheDir / s"vga-monitor-$ver-$plat"
        try {
          if (!tarball.exists) {
            val url =
              java.net.URI.create(s"https://github.com/$repo/releases/download/v$ver/$asset").toURL
            log.info(s"[vga_monitor] downloading $asset")
            val in = url.openStream()
            try {
              java.nio.file.Files.copy(
                in, tarball.toPath, java.nio.file.StandardCopyOption.REPLACE_EXISTING
              )
            } finally {
              in.close()
            }
          }
          if (!srcDir.exists) {
            val rc = scala.sys.process.Process(
              Seq("tar", "-xzf", tarball.getAbsolutePath, "-C", cacheDir.getAbsolutePath)
            ).!
            if (rc != 0) sys.error(s"tar extraction failed for $asset (exit $rc)")
          }
          val platRes = baseRes / plat
          IO.createDirectory(platRes)
          srcDir.listFiles.filter(_.isFile).foreach { f =>
            val n = f.getName
            if (n != "LICENSE" && n != "VERSION") {
              if (isHdl(n)) {
                // HDL wrappers are identical across platforms; copy them once into the IP root.
                if (!hdlCopied) {
                  val dest = baseRes / n
                  IO.copyFile(f, dest)
                  generated += dest
                }
              } else {
                val dest = platRes / n
                IO.copyFile(f, dest)
                generated += dest
              }
            }
          }
          hdlCopied = true
        } catch {
          case scala.util.control.NonFatal(e) =>
            log.warn(s"[vga_monitor] skipping $plat: ${e.getMessage}")
        }
      }
      if (!hdlCopied)
        log.warn("[vga_monitor] no platform bundle could be fetched; IP resources are incomplete")
      generated.toSeq
    }.taskValue
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
