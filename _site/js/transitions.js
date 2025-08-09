// Page transition handler
(function () {
  'use strict';

  // Initialize page with fade-in
  function initPage() {
    const main = document.querySelector('main');
    if (main) {
      // Force reflow then fade in
      requestAnimationFrame(function () {
        main.classList.add('fade-in');
      });
    }
  }

  // Handle page load
  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', initPage);
  } else {
    initPage();
  }

  // Handle link clicks for smooth transitions
  document.addEventListener('click', function (e) {
    const link = e.target.closest('a');

    // Skip if not a link or external link
    if (
      !link ||
      link.hostname !== window.location.hostname ||
      link.getAttribute('href').startsWith('#') ||
      link.getAttribute('target') === '_blank' ||
      link.getAttribute('href').startsWith('mailto:') ||
      link.getAttribute('href').startsWith('tel:')
    ) {
      return;
    }

    e.preventDefault();

    // Start fade out
    const main = document.querySelector('main');
    if (main) {
      main.classList.add('fade-out');
    }

    // Navigate after fade-out completes
    setTimeout(function () {
      window.location.href = link.href;
    }, 800);
  });

  // Handle browser navigation
  window.addEventListener('pageshow', function (e) {
    if (e.persisted) {
      initPage();
    }
  });
})();
