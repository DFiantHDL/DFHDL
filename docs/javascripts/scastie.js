document.addEventListener('DOMContentLoaded', function() {
  let dfhdlVersion = "0.3.7";
  let scalaVersion = "3.4.0";

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

  function getScastieTheme() {
      return document.body.getAttribute('data-md-color-scheme') === 'default' ? 'light' : 'dark';
  }

  function updateScastieTheme() {
      var scastieDiv = document.querySelector('.scastie.embedded');
      // if scastie is already embedded, then we need to remove the DOM
      if (scastieDiv) {
          scastieDiv.remove(); 
      }
      var settings = {
          theme: getScastieTheme(),
          isWorksheetMode: false,
          sbtConfig: sbtConfig,
          targetType: 'scala3',
          scalaVersion: scalaVersion
      };
      scastie.Embedded('.scastie', settings);
  }

  updateScastieTheme(); // Set theme on initial load

  // Listen for changes in theme
  const observer = new MutationObserver(mutations => {
      mutations.forEach(mutation => {
          if (mutation.type === 'attributes' && mutation.attributeName === 'data-md-color-scheme') {
              updateScastieTheme(); // Update theme when MkDocs Material theme changes
          }
      });
  });

  observer.observe(document.body, {
      attributes: true // Configure it to listen to attribute changes
  });
});
