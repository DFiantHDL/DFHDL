var settings = {
  theme: 'light',
  isWorksheetMode: false,
  sbtConfig: `
    scalacOptions ++= Seq(
      "-deprecation",
      "-feature",
      "-language:reflectiveCalls",
      "-language:existentials",
      "-language:implicitConversions",
      "-language:higherKinds",
      "-Ymacro-annotations"
    )
    libraryDependencies ++= Seq(
      "io.github.dfianthdl" %% "dfiant" % "0.1.2"
    )
  `,
  targetType: 'jvm',
  scalaVersion: '2.13.5'
}
window.addEventListener('load', function() {
  scastie.Embedded('.scastie', settings);
});