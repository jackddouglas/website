(function () {
  'use strict';

  let currentImageIndex = 0;
  let images = [];
  let isScrolling = false;

  function initGallery() {
    const galleryImages = document.querySelectorAll('.gallery-image');
    if (galleryImages.length === 0) return;

    images = Array.from(galleryImages);

    // Lock page scroll when gallery is active
    document.body.style.overflow = 'hidden';

    // Add click handler for navigation
    const container = document.querySelector('.gallery');
    if (container) {
      container.addEventListener('click', nextImage);
    }

    // Add scroll handler for navigation
    document.addEventListener('wheel', handleScroll, { passive: false });
    document.addEventListener('touchstart', handleTouchStart, {
      passive: true,
    });
    document.addEventListener('touchmove', handleTouchMove, { passive: false });

    // Add keyboard navigation
    document.addEventListener('keydown', handleKeydown);

    // Prevent context menu on images
    images.forEach((img) => {
      img.addEventListener('contextmenu', (e) => e.preventDefault());
    });
  }

  function nextImage() {
    images[currentImageIndex].classList.remove('active');
    currentImageIndex = (currentImageIndex + 1) % images.length;
    images[currentImageIndex].classList.add('active');
  }

  function prevImage() {
    images[currentImageIndex].classList.remove('active');
    currentImageIndex = (currentImageIndex - 1 + images.length) % images.length;
    images[currentImageIndex].classList.add('active');
  }

  function handleScroll(e) {
    if (isScrolling) return;

    e.preventDefault();
    isScrolling = true;

    if (e.deltaY > 0) {
      nextImage();
    } else {
      prevImage();
    }

    // Throttle scroll events
    setTimeout(() => {
      isScrolling = false;
    }, 300);
  }

  let touchStartY = 0;
  let touchStartX = 0;

  function handleTouchStart(e) {
    touchStartY = e.touches[0].clientY;
    touchStartX = e.touches[0].clientX;
  }

  function handleTouchMove(e) {
    if (isScrolling) return;

    const touchEndY = e.touches[0].clientY;
    const touchEndX = e.touches[0].clientX;
    const deltaY = touchStartY - touchEndY;
    const deltaX = touchStartX - touchEndX;

    // Only trigger if the movement is primarily vertical and significant
    if (Math.abs(deltaY) > Math.abs(deltaX) && Math.abs(deltaY) > 50) {
      e.preventDefault();
      isScrolling = true;

      if (deltaY > 0) {
        nextImage();
      } else {
        prevImage();
      }

      setTimeout(() => {
        isScrolling = false;
      }, 300);
    }
  }

  function handleKeydown(e) {
    switch (e.key) {
      case 'ArrowRight':
      case 'ArrowDown':
      case ' ':
        e.preventDefault();
        nextImage();
        break;
      case 'ArrowLeft':
      case 'ArrowUp':
        e.preventDefault();
        prevImage();
        break;
      case 'Escape':
        // Could add exit functionality here if needed
        break;
    }
  }

  // Initialize when DOM is ready
  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', initGallery);
  } else {
    initGallery();
  }
})();
