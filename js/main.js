// Analog Clock Functionality
function updateClock() {
  const now = new Date();
  const seconds = now.getSeconds();
  const minutes = now.getMinutes();
  const hours = now.getHours() % 12;

  // Calculate rotation angles
  const secondDegrees = (seconds / 60) * 360 - 90;
  const minuteDegrees = ((minutes + seconds / 60) / 60) * 360 - 90;
  const hourDegrees = ((hours + minutes / 60) / 12) * 360 - 90;

  // Apply rotations
  document.querySelector('.second-hand').style.transform = 'translateX(0) rotate(' + secondDegrees + 'deg)';
  document.querySelector('.minute-hand').style.transform = 'translateX(0) rotate(' + minuteDegrees + 'deg)';
  document.querySelector('.hour-hand').style.transform = 'translateX(0) rotate(' + hourDegrees + 'deg)';
}

// Function to position sidenotes at the same vertical position as their references
function repositionSidenotes() {
  const footnoteRefs = document.querySelectorAll('a.footnote-ref');
  const sidenotes = document.querySelectorAll('.sidenote');

  if (footnoteRefs.length === sidenotes.length) {
    for (let i = 0; i < footnoteRefs.length; i++) {
      const ref = footnoteRefs[i];
      const sidenote = sidenotes[i];
      const container = sidenote.parentElement;

      if (container && container.classList.contains('sidenote-container')) {
        const refRect = ref.getBoundingClientRect();
        const containerRect = container.getBoundingClientRect();
        const topPosition = refRect.top - containerRect.top;

        sidenote.style.top = topPosition + 'px';
      }
    }
  }
}

// Handle responsive footnotes
function handleResponsiveFootnotes() {
  const sidenotes = document.querySelectorAll('.sidenote');
  const footnotes = document.querySelectorAll('.footnotes');

  if (window.innerWidth >= 1024) {
    // Show sidenotes, hide footnotes
    sidenotes.forEach(note => note.style.display = 'block');
    footnotes.forEach(section => section.style.display = 'none');

    // Reposition sidenotes
    repositionSidenotes();
  } else {
    // Hide sidenotes, show footnotes
    sidenotes.forEach(note => note.style.display = 'none');
    footnotes.forEach(section => section.style.display = 'block');
  }
}

// Initialize when DOM is fully loaded
document.addEventListener('DOMContentLoaded', function() {
  // Update clock every second
  setInterval(updateClock, 1000);

  // Initialize clock immediately
  updateClock();

  // Handle window resize for responsive footnotes
  window.addEventListener('resize', handleResponsiveFootnotes);

  // Initial call to handle footnotes
  handleResponsiveFootnotes();

  // Call repositionSidenotes when images load, as they can affect layout
  window.addEventListener('load', function () {
    if (window.innerWidth >= 1024) {
      repositionSidenotes();
    }
  });
});
