let dfhdlVersion = "0.9.1";
let scalaVersion = "3.6.3";

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

function updateCodeBlocksAndDetails(preferredLanguage) {
  var codeBlocks = document.querySelectorAll('.scastie'); // Assuming all targeted code blocks have this class
  codeBlocks.forEach(function(codeBlock) {
    let content = codeBlock.textContent;
    if (preferredLanguage === "Verilog") {
      content = content.replace(/(backends\.)vhdl/g, '$1verilog');
    } else { // VHDL
      content = content.replace(/(backends\.)verilog/g, '$1vhdl');
    }
    codeBlock.textContent = content;
  });
  if (codeBlocks.length > 0) {
    updateScastieTheme();
  }
  // Open/close details based on preferred language
  if (preferredLanguage === "Verilog") {
    var details = document.querySelectorAll('.verilog'); 
    details.forEach(function(div){
      div.setAttribute("open","true");
    })
    var details = document.querySelectorAll('.vhdl'); 
    details.forEach(function(div){
      div.removeAttribute("open");
    })
    //select tabs with Verilog
    const tabLabels = document.querySelectorAll(
      ".tabbed-set > label, .tabbed-alternate > .tabbed-labels > label"
    );
    tabLabels.forEach(label => {
      if (label.textContent.includes("Verilog")) {
        label.click();
      }
    });
  } else { // VHDL
    var details = document.querySelectorAll('.vhdl'); 
    details.forEach(function(div){
      div.setAttribute("open","true");
    })
    var details = document.querySelectorAll('.verilog'); 
    details.forEach(function(div){
      div.removeAttribute("open");
    })
    //select tabs with VHDL
    const tabLabels = document.querySelectorAll(
      ".tabbed-set > label, .tabbed-alternate > .tabbed-labels > label"
    );
    tabLabels.forEach(label => {
      if (label.textContent.includes("VHDL")) {
        label.click();
      }
    });
  }
}

// Set preferred language tab on page load
window.addEventListener("load", () => {
  var savedLanguage = localStorage.getItem('preferredLanguage');
  const tabLabels = document.querySelectorAll(
    ".tabbed-set > label, .tabbed-alternate > .tabbed-labels > label"
  );
  tabLabels.forEach(label => {
    if (label.textContent.includes(savedLanguage)) {
      label.click();
    }
  });
});

document.addEventListener('DOMContentLoaded', function() {
  var currentLang = document.getElementById('current-language');
  var savedLanguage = localStorage.getItem('preferredLanguage');
  var languageToggleButton = document.getElementById('language-toggle-button');

  languageToggleButton.addEventListener('click', function() {
    var preferredLanguage = currentLang.textContent === "Verilog" ? "VHDL" : "Verilog";
    localStorage.setItem('preferredLanguage', preferredLanguage);
    currentLang.textContent = preferredLanguage;
    updateCodeBlocksAndDetails(preferredLanguage);
  });
  
  if (savedLanguage) {
    currentLang.textContent = savedLanguage;
    updateCodeBlocksAndDetails(savedLanguage);
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
