let dfhdlVersion = "0.3.3";
let scalaVersion = "3.3.0";

let sbtConfig = `
val dfhdlVersion = "${dfhdlVersion}"
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
resolvers += Resolver.sonatypeRepo("snapshots")
`;

var settings = {
  theme: 'light',
  isWorksheetMode: false,
  sbtConfig: sbtConfig,
  targetType: 'scala3',
  scalaVersion: scalaVersion
}
window.addEventListener('load', function() {
  scastie.Embedded('.scastie', settings);
});