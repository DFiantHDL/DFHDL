var settings = {
  theme: 'light',
  isWorksheetMode: false,
  sbtConfig: `
val dfhdlVersion = "0.3.3"
scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
  "-language:implicitConversions",
  "-language:strictEquality",
)
addCompilerPlugin(
  "io.github.dfianthdl" % "dfhdl-plugin" % dfhdlVersion cross CrossVersion.full
)
libraryDependencies ++= Seq(
  "io.github.dfianthdl" %% "dfhdl" % dfhdlVersion
)
  `,
  targetType: 'scala3',
  scalaVersion: '3.3.0'
}
window.addEventListener('load', function() {
  scastie.Embedded('.scastie', settings);
});