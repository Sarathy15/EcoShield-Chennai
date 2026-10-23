/**
 * EcoShield – Environmental Intelligence Dashboard
 * Real-world Professional Frontend Utilities
 */

(function () {
  'use strict';

  // --- Keyboard Shortcuts (1-6 for instant tab switching) ---
  function setupShortcuts() {
    document.addEventListener('keydown', function (e) {
      // Don't intercept if user is typing in an input or textarea
      const tag = (e.target && e.target.tagName) ? e.target.tagName.toLowerCase() : '';
      if (tag === 'input' || tag === 'textarea' || tag === 'select') return;

      // Quick Tab Switch: '1' through '6'
      if (e.key >= '1' && e.key <= '6') {
        const tabs = document.querySelectorAll('.navbar-nav .nav-link');
        const idx = parseInt(e.key, 10) - 1;
        if (tabs[idx]) {
          tabs[idx].click();
        }
      }
    });
  }

  // --- Notification Toast (Optional utility) ---
  function showToast(message) {
    let toast = document.getElementById('es-toast');
    if (!toast) {
      toast = document.createElement('div');
      toast.id = 'es-toast';
      toast.style.cssText = `
        position: fixed;
        bottom: 24px;
        right: 24px;
        background: #0f172a;
        color: #ffffff;
        padding: 9px 18px;
        border-radius: 6px;
        font-family: 'Inter', sans-serif;
        font-size: 0.82rem;
        font-weight: 600;
        box-shadow: 0 4px 14px rgba(0, 0, 0, 0.16);
        z-index: 99999;
        transition: opacity 0.25s ease, transform 0.25s ease;
        opacity: 0;
        transform: translateY(8px);
        pointer-events: none;
      `;
      document.body.appendChild(toast);
    }
    toast.textContent = message;
    toast.style.opacity = '1';
    toast.style.transform = 'translateY(0)';
    setTimeout(function () {
      toast.style.opacity = '0';
      toast.style.transform = 'translateY(8px)';
    }, 2000);
  }

  // Ensure active nav-item is visible when scrolled horizontally on mobile
  function setupNavScroll() {
    document.addEventListener('click', function (e) {
      const link = e.target.closest('.navbar-nav .nav-link');
      if (link) {
        setTimeout(function() {
          link.scrollIntoView({ behavior: 'smooth', block: 'nearest', inline: 'center' });
        }, 50);
      }
    });
  }

  // Auto-resize Plotly charts and Leaflet maps on window resize and orientation change
  function setupResponsiveResize() {
    let resizeTimer;
    function handleResize() {
      clearTimeout(resizeTimer);
      resizeTimer = setTimeout(function () {
        const plotlyDivs = document.querySelectorAll('.js-plotly-plot');
        plotlyDivs.forEach(function (el) {
          if (window.Plotly && window.Plotly.Plots && window.Plotly.Plots.resize) {
            try {
              window.Plotly.Plots.resize(el);
            } catch (e) {}
          }
        });
        const leafletMaps = document.querySelectorAll('.leaflet-container');
        leafletMaps.forEach(function (el) {
          if (el._leaflet_map) {
            try {
              el._leaflet_map.invalidateSize();
            } catch (e) {}
          }
        });
      }, 100);
    }

    window.addEventListener('resize', handleResize);
    window.addEventListener('orientationchange', function () {
      setTimeout(handleResize, 100);
      setTimeout(handleResize, 300);
    });
  }

  // Trigger chart refit immediately when switching tabs
  function setupTabSwitchFix() {
    document.addEventListener('shown.bs.tab', function () {
      setTimeout(function () {
        window.dispatchEvent(new Event('resize'));
      }, 60);
      setTimeout(function () {
        window.dispatchEvent(new Event('resize'));
      }, 250);
    });

    document.querySelectorAll('.navbar-nav .nav-link').forEach(function (link) {
      link.addEventListener('click', function () {
        setTimeout(function () {
          window.dispatchEvent(new Event('resize'));
        }, 80);
        setTimeout(function () {
          window.dispatchEvent(new Event('resize'));
        }, 300);
      });
    });
  }

  function init() {
    setupShortcuts();
    setupNavScroll();
    setupResponsiveResize();
    setupTabSwitchFix();
  }

  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', init);
  } else {
    init();
  }

  window.ecoToast = showToast;
})();
