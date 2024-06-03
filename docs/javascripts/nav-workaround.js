// workaround issue (or similar) https://github.com/squidfunk/mkdocs-material/issues/6726
document.addEventListener('DOMContentLoaded', function() {
  // Select all input elements with both 'md-nav__toggle' and 'md-toggle--indeterminate' classes
  const elements = document.querySelectorAll('input.md-nav__toggle.md-toggle--indeterminate');

  // Loop through each element and remove the 'md-toggle--indeterminate' class
  elements.forEach(function(element) {
      element.classList.remove('md-toggle--indeterminate');
  });
});
