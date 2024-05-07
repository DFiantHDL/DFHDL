document.addEventListener('DOMContentLoaded', function() {
  function getComputedWidth(element) {
    return element.getBoundingClientRect().width;
  }

  function resizeSVG(svg, width) {
    svg.attr("width", width).attr("height", "auto");
  }

  function setupSVG(preWidth, jsonStr, replaceElement) {
    // Create SVG element
    let svgElement = document.createElementNS("http://www.w3.org/2000/svg", "svg");
    svgElement.setAttribute('width', preWidth);
    svgElement.setAttribute('height', 'auto');
    svgElement.classList.add('hwschematic');

    replaceElement.parentNode.replaceChild(svgElement, replaceElement);

    let svg = d3.select(svgElement);
    let hwSchematic = new d3.HwSchematic(svg);
    let zoom = d3.zoom().on("zoom", function applyTransform(ev) {
      hwSchematic.root.attr("transform", ev.transform);
    });

    svg.call(zoom).on("dblclick.zoom", null);

    function displayJsonObj(jsonObj) {
      hwSchematic.bindData(jsonObj).then(() => {}, (e) => {
        hwSchematic.setErrorText(e);
        throw e;
      });
    }

    function displayJsonStr(jsonStr) {
      try {
        let jsonObj = JSON.parse(jsonStr);
        displayJsonObj(jsonObj);
      } catch (e) {
        hwSchematic.setErrorText(e);
        throw e;
      }
    }

    // Display JSON string
    displayJsonStr(jsonStr);

    // Resize listener
    window.addEventListener('resize', function () {
      let currentWidth = getComputedWidth(svgElement.parentNode);
      resizeSVG(svg, currentWidth);
    });
  }

  // Handle pre.hwschematic elements
  document.querySelectorAll('pre.hwschematic').forEach(function(preElement) {
    let jsonStr = preElement.querySelector('code').textContent;
    let preWidth = getComputedWidth(preElement);
    setupSVG(preWidth, jsonStr, preElement);
  });

  // Handle img.hwschematic elements
  document.querySelectorAll('img.hwschematic').forEach(function(imgElement) {
    let src = imgElement.getAttribute('src');
    let specifiedWidth = imgElement.getAttribute('width');
    let imgWidth = specifiedWidth ? parseInt(specifiedWidth, 10) : getComputedWidth(imgElement.parentNode);
    fetch(src)
      .then(response => response.json())
      .then(jsonData => {
        let jsonStr = JSON.stringify(jsonData);
        setupSVG(imgWidth, jsonStr, imgElement);
      })
      .catch(error => {
        console.error('Error loading JSON from:', src, error);
      });
  });
});

// Old code before supporting images too

// document.addEventListener('DOMContentLoaded', function() {
//   function viewport() {
//     let e = window,
//         a = 'inner';
//     if (!('innerWidth' in window)) {
//         a = 'client';
//         e = document.documentElement || document.body;
//     }
//     return { width: e[a + 'Width'], height: e[a + 'Height'] }
//   }

//   // Set up resize handling
//   function resizeSVG(svg) {
//     let size = viewport();
//     svg;//.attr("width", size.width);//.attr("height", size.height);
//   }

//   // Function to get computed style dimension
//   function getComputedWidth(element) {
//     return element.getBoundingClientRect().width;
//   }

//   // Convert each hwschematic code block to an SVG and apply JSON
//   document.querySelectorAll('pre.hwschematic').forEach(function(preElement) {
//     let jsonStr = preElement.querySelector('code').textContent;
//     let preWidth = getComputedWidth(preElement); // Get the width of the <pre> element

//     // Create an SVG element and replace the <pre> element with it
//     let svgElement = document.createElementNS("http://www.w3.org/2000/svg", "svg");
//     svgElement.setAttribute('width', preWidth);
//     // svgElement.setAttribute('height', viewport().height);
//     svgElement.classList.add('hwschematic');
//     preElement.parentNode.replaceChild(svgElement, preElement);

//     let svg = d3.select(svgElement);
//     let hwSchematic = new d3.HwSchematic(svg);
//     let zoom = d3.zoom().on("zoom", function applyTransform(ev) {
//       hwSchematic.root.attr("transform", ev.transform);
//     });

//     svg.call(zoom).on("dblclick.zoom", null);

//     function displayJsonObj(jsonObj) {
//       hwSchematic.bindData(jsonObj).then(() => {}, (e) => {
//         hwSchematic.setErrorText(e);
//         throw e;
//       });
//     }

//     function displayJsonStr(jsonStr) {
//       try {
//         let jsonObj = JSON.parse(jsonStr);
//         displayJsonObj(jsonObj);
//       } catch (e) {
//         hwSchematic.setErrorText(e);
//         throw e;
//       }
//     }

//     // Display JSON string
//     displayJsonStr(jsonStr);

//     // Resize listener
//     let orig = window.onresize;
//     window.onresize = function (ev) {
//         if (orig) orig(ev);
//         resizeSVG(svg);
//     }
//   });
// });