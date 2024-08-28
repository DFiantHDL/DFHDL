let dfhdlVersion = "0.7.1";
let scalaVersion = "3.5.0";

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
  var scastieDivs = document.querySelectorAll('.scastie.embedded');
  // if scastie is already embedded, then we need to remove the DOM
  scastieDivs.forEach(function(div) {
      div.remove();
  });
  var settings = {
      theme: getScastieTheme(),
      isWorksheetMode: false,
      sbtConfig: sbtConfig,
      targetType: 'scala3',
      scalaVersion: scalaVersion
  };
  scastie.Embedded('.scastie', settings);
}

function updateCodeBlocks(preferredLanguage) {
  var codeBlocks = document.querySelectorAll('.scastie'); // Assuming all targeted code blocks have this class
  codeBlocks.forEach(function(codeBlock) {
    let content = codeBlock.textContent;
    if (preferredLanguage === "Verilog") {
      content = content.replace(/(backends\.)vhdl/g, '$1verilog');
    } else {
      content = content.replace(/(backends\.)verilog/g, '$1vhdl');
    }
    codeBlock.textContent = content;
  });
  if (codeBlocks.length > 0) {
    updateScastieTheme();
  }
}

document.addEventListener('DOMContentLoaded', function() {
  var currentLang = document.getElementById('current-language');
  var savedLanguage = localStorage.getItem('preferredLanguage');
  var languageToggleButton = document.getElementById('language-toggle-button');

  languageToggleButton.addEventListener('click', function() {
    var preferredLanguage = currentLang.textContent === "Verilog" ? "VHDL" : "Verilog";
    localStorage.setItem('preferredLanguage', preferredLanguage);
    currentLang.textContent = preferredLanguage;
    updateCodeBlocks(preferredLanguage);
  });
  
  if (savedLanguage) {
    currentLang.textContent = savedLanguage;
    updateCodeBlocks(savedLanguage);
  }
  else {
    updateScastieTheme(); // Set theme on initial load
  }

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
